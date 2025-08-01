#!/usr/bin/env bash
# shellcheck disable=SC2086 # we want word splitting
# shellcheck disable=SC1091 # paths only become valid at runtime

. "${SCRIPTS_DIR}/setup-test-env.sh"

section_start test_setup "deqp: preparing test setup"

set -ex

# Needed so configuration files can contain paths to files in /install
ln -sf "$CI_PROJECT_DIR"/install /install

if [ -z "$GPU_VERSION" ]; then
   echo 'GPU_VERSION must be set to something like "llvmpipe" or "freedreno-a630" (the name used in .gitlab-ci/gpu-version-*.txt)'
   exit 1
fi

INSTALL=$(realpath -s "$PWD"/install)

# Set up the driver environment.
export LD_LIBRARY_PATH="$INSTALL"/lib/:$LD_LIBRARY_PATH
export EGL_PLATFORM=surfaceless
ARCH=$(uname -m)
export VK_DRIVER_FILES="$INSTALL"/share/vulkan/icd.d/"$VK_DRIVER"_icd."$ARCH".json
export OCL_ICD_VENDORS="$INSTALL"/etc/OpenCL/vendors/

if [ -n "${ANGLE_TAG:-}" ]; then
  # Are we using the right ANGLE version?
  ci_tag_test_time_check "ANGLE_TAG"
  export LD_LIBRARY_PATH=/angle:$LD_LIBRARY_PATH
fi

if [ -n "${FLUSTER_TAG:-}" ]; then
  # Are we using the right Fluster version?
  ci_tag_test_time_check "FLUSTER_TAG"
  export LIBVA_DRIVERS_PATH=$INSTALL/lib/dri/
  # libva spams driver open info by default, and that happens per testcase.
  export LIBVA_MESSAGING_LEVEL=1
fi

if [ -n "$PIGLIT_TAG" ]; then
  # Are we using the right Piglit version?
  ci_tag_test_time_check "PIGLIT_TAG"
elif [ -d "/piglit" ]; then
  # The job does not inherit from .test-piglit, so we remove it.
  # This makes sure that we can both do the right version checks when needed,
  # and also optimise our dependencies so we don't pull unneeded stuff.
  rm -r /piglit
fi

# Ensure Mesa Shader Cache resides on tmpfs.
SHADER_CACHE_HOME=${XDG_CACHE_HOME:-${HOME}/.cache}
SHADER_CACHE_DIR=${MESA_SHADER_CACHE_DIR:-${SHADER_CACHE_HOME}/mesa_shader_cache}

findmnt -n tmpfs ${SHADER_CACHE_HOME} || findmnt -n tmpfs ${SHADER_CACHE_DIR} || {
    mkdir -p ${SHADER_CACHE_DIR}
    mount -t tmpfs -o nosuid,nodev,size=2G,mode=1755 tmpfs ${SHADER_CACHE_DIR}
}

BASELINE=""
if [ -e "$INSTALL/$GPU_VERSION-fails.txt" ]; then
    BASELINE="--baseline $INSTALL/$GPU_VERSION-fails.txt"
fi

# Default to an empty known flakes file if it doesn't exist.
touch $INSTALL/$GPU_VERSION-flakes.txt


if [ -n "$VK_DRIVER" ] && [ -e "$INSTALL/$VK_DRIVER-skips.txt" ]; then
    DEQP_SKIPS="$DEQP_SKIPS $INSTALL/$VK_DRIVER-skips.txt"
fi

if [ -n "$GALLIUM_DRIVER" ] && [ -e "$INSTALL/$GALLIUM_DRIVER-skips.txt" ]; then
    DEQP_SKIPS="$DEQP_SKIPS $INSTALL/$GALLIUM_DRIVER-skips.txt"
fi

if [ -n "$DRIVER_NAME" ] && [ -e "$INSTALL/$DRIVER_NAME-skips.txt" ]; then
    DEQP_SKIPS="$DEQP_SKIPS $INSTALL/$DRIVER_NAME-skips.txt"
fi

if [ -e "$INSTALL/$GPU_VERSION-skips.txt" ]; then
    DEQP_SKIPS="$DEQP_SKIPS $INSTALL/$GPU_VERSION-skips.txt"
fi

if [ -e "$INSTALL/$GPU_VERSION-slow-skips.txt" ] && [[ $CI_JOB_NAME != *full* ]]; then
    DEQP_SKIPS="$DEQP_SKIPS $INSTALL/$GPU_VERSION-slow-skips.txt"
fi

if [ -n "${ANGLE_TAG:-}" ]; then
    DEQP_SKIPS="$DEQP_SKIPS $INSTALL/angle-skips.txt"
fi

# Set the path to VK validation layer settings (in case it ends up getting loaded)
# Note: If you change the format of this filename, look through the rest of the
# tree for other places that need to be kept in sync (e.g.
# src/gallium/drivers/zink/ci/gitlab-ci-inc.yml)
export VK_LAYER_SETTINGS_PATH=$INSTALL/$GPU_VERSION-validation-settings.txt

report_load() {
    echo "System load: $(cut -d' ' -f1-3 < /proc/loadavg)"
    echo "# of CPU cores: $(grep -c processor /proc/cpuinfo)"
}

if [ "$GALLIUM_DRIVER" = "virpipe" ]; then
    # deqp is to use virpipe, and virgl_test_server llvmpipe
    export GALLIUM_DRIVER="$GALLIUM_DRIVER"

    VTEST_ARGS="--use-egl-surfaceless"
    if [ "$VIRGL_HOST_API" = "GLES" ]; then
        VTEST_ARGS="$VTEST_ARGS --use-gles"
    fi

    GALLIUM_DRIVER=llvmpipe \
    virgl_test_server $VTEST_ARGS >$RESULTS_DIR/vtest-log.txt 2>&1 &

    sleep 1
fi

uncollapsed_section_switch deqp "deqp: deqp-runner"

# Print the detailed version with the list of backports and local patches
{ set +x; } 2>/dev/null
for api in vk-main vk gl gles; do
  deqp_version_log=/deqp-$api/deqp-$api-version
  if [ -r "$deqp_version_log" ]; then
    cat "$deqp_version_log"
  fi
done
set -x

# If you change the format of the suite toml filenames or the
# $GPU_VERSION-{fails,flakes,skips}.txt filenames, look through the rest
# of the tree for other places that need to be kept in sync (e.g.
# src/**/ci/gitlab-ci*.yml)
set +e
deqp-runner -V
deqp-runner \
    suite \
    --suite $INSTALL/deqp-$DEQP_SUITE.toml \
    --output $RESULTS_DIR \
    --skips $INSTALL/all-skips.txt $DEQP_SKIPS \
    --flakes $INSTALL/$GPU_VERSION-flakes.txt \
    --testlog-to-xml /deqp-tools/testlog-to-xml \
    --fraction-start ${CI_NODE_INDEX:-1} \
    --fraction $((CI_NODE_TOTAL * ${DEQP_FRACTION:-1})) \
    --jobs ${FDO_CI_CONCURRENT:-4} \
    $BASELINE \
    ${DEQP_RUNNER_MAX_FAILS:+--max-fails "$DEQP_RUNNER_MAX_FAILS"} \
    ${DEQP_FORCE_ASAN:+--env LD_PRELOAD=libasan.so.8:/install/lib/libdlclose-skip.so}; DEQP_EXITCODE=$?

{ set +x; } 2>/dev/null

set -e

section_switch test_post_process "deqp: post-processing test results"
set -x

report_load

# Remove all but the first 50 individual XML files uploaded as artifacts, to
# save fd.o space when you break everything.
find $RESULTS_DIR -name \*.xml | \
    sort -n |
    sed -n '1,+49!p' | \
    xargs rm -f

# If any QPA XMLs are there, then include the XSL/CSS in our artifacts.
find $RESULTS_DIR -name \*.xml \
    -exec cp /deqp-tools/testlog.css /deqp-tools/testlog.xsl "$RESULTS_DIR/" ";" \
    -quit

deqp-runner junit \
   --testsuite dEQP \
   --results $RESULTS_DIR/failures.csv \
   --output $RESULTS_DIR/junit.xml \
   --limit 50 \
   --template "See $ARTIFACTS_BASE_URL/results/{{testcase}}.xml"

# Report the flakes to the IRC channel for monitoring (if configured):
if [ -n "$FLAKES_CHANNEL" ]; then
  python3 $INSTALL/report-flakes.py \
         --host irc.oftc.net \
         --port 6667 \
         --results $RESULTS_DIR/results.csv \
         --known-flakes $INSTALL/$GPU_VERSION-flakes.txt \
         --channel "$FLAKES_CHANNEL" \
         --runner "$CI_RUNNER_DESCRIPTION" \
         --job "$CI_JOB_ID" \
         --url "$CI_JOB_URL" \
         --branch "${CI_MERGE_REQUEST_SOURCE_BRANCH_NAME:-$CI_COMMIT_BRANCH}" \
         --branch-title "${CI_MERGE_REQUEST_TITLE:-$CI_COMMIT_TITLE}" || true
fi

# Compress results.csv to save on bandwidth during the upload of artifacts to
# GitLab. This reduces the size in a VKCTS run from 135 to 7.6MB, and takes
# 0.17s on a Ryzen 5950X (16 threads, 0.95s when limited to 1 thread).
zstd --quiet --rm --threads ${FDO_CI_CONCURRENT:-0} -8 "$RESULTS_DIR/results.csv" -o "$RESULTS_DIR/results.csv.zst"

set +x
section_end test_post_process

exit $DEQP_EXITCODE
