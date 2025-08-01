// Copyright © 2022 Collabora, Ltd.
// SPDX-License-Identifier: MIT

#![allow(non_upper_case_globals)]

use crate::api::GetDebugFlags;
use crate::api::DEBUG;
use crate::builder::*;
use crate::ir::*;
use crate::sph::{OutputTopology, PixelImap};

use nak_bindings::*;

use compiler::bindings::*;
use compiler::cfg::CFGBuilder;
use compiler::nir::*;
use compiler::nir_instr_printer::NirInstrPrinter;
use rustc_hash::{FxBuildHasher, FxHashMap, FxHashSet};
use std::cmp::max;
use std::ops::Index;

fn init_info_from_nir(nak: &nak_compiler, nir: &nir_shader) -> ShaderInfo {
    ShaderInfo {
        max_warps_per_sm: 0,
        num_gprs: 0,
        num_instrs: 0,
        num_static_cycles: 0,
        num_spills_to_mem: 0,
        num_fills_from_mem: 0,
        num_spills_to_reg: 0,
        num_fills_from_reg: 0,
        num_control_barriers: 0,
        slm_size: nir.scratch_size,
        max_crs_depth: 0,
        uses_global_mem: false,
        writes_global_mem: false,
        // TODO: handle this.
        uses_fp64: false,
        stage: match nir.info.stage() {
            MESA_SHADER_COMPUTE => {
                ShaderStageInfo::Compute(ComputeShaderInfo {
                    local_size: [
                        nir.info.workgroup_size[0],
                        nir.info.workgroup_size[1],
                        nir.info.workgroup_size[2],
                    ],
                    smem_size: nir.info.shared_size.try_into().unwrap(),
                })
            }
            MESA_SHADER_VERTEX => ShaderStageInfo::Vertex,
            MESA_SHADER_FRAGMENT => {
                let info_fs = unsafe { &nir.info.__bindgen_anon_1.fs };
                ShaderStageInfo::Fragment(FragmentShaderInfo {
                    uses_kill: false,
                    does_interlock: false,
                    post_depth_coverage: info_fs.post_depth_coverage(),
                    early_fragment_tests: info_fs.early_fragment_tests(),
                    uses_sample_shading: info_fs.uses_sample_shading(),
                })
            }
            MESA_SHADER_GEOMETRY => {
                let info_gs = unsafe { &nir.info.__bindgen_anon_1.gs };
                let output_topology = match info_gs.output_primitive {
                    MESA_PRIM_POINTS => OutputTopology::PointList,
                    MESA_PRIM_LINE_STRIP => OutputTopology::LineStrip,
                    MESA_PRIM_TRIANGLE_STRIP => OutputTopology::TriangleStrip,
                    _ => panic!(
                        "Invalid GS input primitive {}",
                        info_gs.input_primitive
                    ),
                };

                ShaderStageInfo::Geometry(GeometryShaderInfo {
                    // TODO: Should be set if VK_NV_geometry_shader_passthrough is in use.
                    passthrough_enable: false,
                    stream_out_mask: info_gs.active_stream_mask(),
                    threads_per_input_primitive: info_gs.invocations,
                    output_topology: output_topology,
                    max_output_vertex_count: info_gs.vertices_out,
                })
            }
            MESA_SHADER_TESS_CTRL => {
                let info_tess = unsafe { &nir.info.__bindgen_anon_1.tess };
                ShaderStageInfo::TessellationInit(TessellationInitShaderInfo {
                    per_patch_attribute_count: 6,
                    threads_per_patch: info_tess.tcs_vertices_out,
                })
            }
            MESA_SHADER_TESS_EVAL => {
                let info_tess = unsafe { &nir.info.__bindgen_anon_1.tess };
                ShaderStageInfo::Tessellation(TessellationShaderInfo {
                    domain: match info_tess._primitive_mode {
                        TESS_PRIMITIVE_TRIANGLES => {
                            TessellationDomain::Triangle
                        }
                        TESS_PRIMITIVE_QUADS => TessellationDomain::Quad,
                        TESS_PRIMITIVE_ISOLINES => TessellationDomain::Isoline,
                        _ => panic!("Invalid tess_primitive_mode"),
                    },
                    spacing: match info_tess.spacing() {
                        TESS_SPACING_EQUAL => TessellationSpacing::Integer,
                        TESS_SPACING_FRACTIONAL_ODD => {
                            TessellationSpacing::FractionalOdd
                        }
                        TESS_SPACING_FRACTIONAL_EVEN => {
                            TessellationSpacing::FractionalEven
                        }
                        _ => panic!("Invalid gl_tess_spacing"),
                    },
                    primitives: if info_tess.point_mode() {
                        TessellationPrimitives::Points
                    } else if info_tess._primitive_mode
                        == TESS_PRIMITIVE_ISOLINES
                    {
                        TessellationPrimitives::Lines
                    } else if info_tess.ccw() {
                        TessellationPrimitives::TrianglesCCW
                    } else {
                        TessellationPrimitives::TrianglesCW
                    },
                })
            }
            _ => panic!("Unknown shader stage"),
        },
        io: match nir.info.stage() {
            MESA_SHADER_COMPUTE => ShaderIoInfo::None,
            MESA_SHADER_FRAGMENT => ShaderIoInfo::Fragment(FragmentIoInfo {
                sysvals_in: SysValInfo {
                    // Required on fragment shaders, otherwise it cause a trap.
                    ab: 1 << 31,
                    c: 0,
                },
                sysvals_in_d: [PixelImap::Unused; 8],
                attr_in: [PixelImap::Unused; 128],
                barycentric_attr_in: [0; 4],
                reads_sample_mask: false,
                writes_color: 0,
                writes_sample_mask: false,
                writes_depth: false,
            }),
            MESA_SHADER_VERTEX
            | MESA_SHADER_GEOMETRY
            | MESA_SHADER_TESS_CTRL
            | MESA_SHADER_TESS_EVAL => {
                let num_clip = nir.info.clip_distance_array_size();
                let num_cull = nir.info.cull_distance_array_size();
                let clip_enable = (1_u32 << num_clip) - 1;
                let cull_enable = ((1_u32 << num_cull) - 1) << num_clip;

                ShaderIoInfo::Vtg(VtgIoInfo {
                    sysvals_in: SysValInfo::default(),
                    sysvals_in_d: 0,
                    sysvals_out: SysValInfo::default(),
                    sysvals_out_d: 0,
                    attr_in: [0; 4],
                    attr_out: [0; 4],

                    // TODO: figure out how to fill this.
                    store_req_start: u8::MAX,
                    store_req_end: 0,

                    clip_enable: clip_enable.try_into().unwrap(),
                    cull_enable: cull_enable.try_into().unwrap(),
                    xfb: if nir.xfb_info.is_null() {
                        None
                    } else {
                        Some(Box::new(unsafe {
                            nak_xfb_from_nir(nak, nir.xfb_info)
                        }))
                    },
                })
            }
            _ => panic!("Unknown shader stage"),
        },
    }
}

fn alloc_ssa_for_nir(b: &mut impl SSABuilder, ssa: &nir_def) -> Vec<SSAValue> {
    let (file, comps) = if ssa.bit_size == 1 {
        (RegFile::Pred, ssa.num_components)
    } else {
        let bits = ssa.bit_size * ssa.num_components;
        (RegFile::GPR, bits.div_ceil(32))
    };

    let mut vec = Vec::new();
    for _ in 0..comps {
        vec.push(b.alloc_ssa(file));
    }
    vec
}

struct PhiAllocMap<'a> {
    alloc: &'a mut PhiAllocator,
    map: FxHashMap<(u32, u8), Phi>,
}

impl<'a> PhiAllocMap<'a> {
    fn new(alloc: &'a mut PhiAllocator) -> PhiAllocMap<'a> {
        PhiAllocMap {
            alloc: alloc,
            map: Default::default(),
        }
    }

    fn get_phi(&mut self, nphi: &nir_phi_instr, comp: u8) -> Phi {
        *self
            .map
            .entry((nphi.def.index, comp))
            .or_insert_with(|| self.alloc.alloc())
    }
}

struct PerSizeFloatControls {
    pub ftz: bool,
    pub rnd_mode: FRndMode,
}

struct ShaderFloatControls {
    pub fp16: PerSizeFloatControls,
    pub fp32: PerSizeFloatControls,
    pub fp64: PerSizeFloatControls,
}

impl Default for ShaderFloatControls {
    fn default() -> Self {
        Self {
            fp16: PerSizeFloatControls {
                ftz: false,
                rnd_mode: FRndMode::NearestEven,
            },
            fp32: PerSizeFloatControls {
                ftz: true, // Default FTZ on fp32
                rnd_mode: FRndMode::NearestEven,
            },
            fp64: PerSizeFloatControls {
                ftz: false,
                rnd_mode: FRndMode::NearestEven,
            },
        }
    }
}

impl ShaderFloatControls {
    fn from_nir(nir: &nir_shader) -> ShaderFloatControls {
        let nir_fc = nir.info.float_controls_execution_mode;
        let mut fc: ShaderFloatControls = Default::default();

        if (nir_fc & FLOAT_CONTROLS_DENORM_PRESERVE_FP16) != 0 {
            fc.fp16.ftz = false;
        } else if (nir_fc & FLOAT_CONTROLS_DENORM_FLUSH_TO_ZERO_FP16) != 0 {
            fc.fp16.ftz = true;
        }
        if (nir_fc & FLOAT_CONTROLS_ROUNDING_MODE_RTE_FP16) != 0 {
            fc.fp16.rnd_mode = FRndMode::NearestEven;
        } else if (nir_fc & FLOAT_CONTROLS_ROUNDING_MODE_RTZ_FP16) != 0 {
            fc.fp16.rnd_mode = FRndMode::Zero;
        }

        if (nir_fc & FLOAT_CONTROLS_DENORM_PRESERVE_FP32) != 0 {
            fc.fp32.ftz = false;
        } else if (nir_fc & FLOAT_CONTROLS_DENORM_FLUSH_TO_ZERO_FP32) != 0 {
            fc.fp32.ftz = true;
        }
        if (nir_fc & FLOAT_CONTROLS_ROUNDING_MODE_RTE_FP32) != 0 {
            fc.fp32.rnd_mode = FRndMode::NearestEven;
        } else if (nir_fc & FLOAT_CONTROLS_ROUNDING_MODE_RTZ_FP32) != 0 {
            fc.fp32.rnd_mode = FRndMode::Zero;
        }

        if (nir_fc & FLOAT_CONTROLS_DENORM_PRESERVE_FP64) != 0 {
            fc.fp64.ftz = false;
        } else if (nir_fc & FLOAT_CONTROLS_DENORM_FLUSH_TO_ZERO_FP64) != 0 {
            fc.fp64.ftz = true;
        }
        if (nir_fc & FLOAT_CONTROLS_ROUNDING_MODE_RTE_FP64) != 0 {
            fc.fp64.rnd_mode = FRndMode::NearestEven;
        } else if (nir_fc & FLOAT_CONTROLS_ROUNDING_MODE_RTZ_FP64) != 0 {
            fc.fp64.rnd_mode = FRndMode::Zero;
        }

        fc
    }
}

fn f_rnd_mode_from_nir(mode: nir_rounding_mode) -> FRndMode {
    match mode {
        nir_rounding_mode_undef | nir_rounding_mode_rtne => {
            FRndMode::NearestEven
        }
        nir_rounding_mode_ru => FRndMode::PosInf,
        nir_rounding_mode_rd => FRndMode::NegInf,
        nir_rounding_mode_rtz => FRndMode::Zero,
        _ => panic!("Invalid NIR rounding mode"),
    }
}

impl Index<FloatType> for ShaderFloatControls {
    type Output = PerSizeFloatControls;

    fn index(&self, idx: FloatType) -> &PerSizeFloatControls {
        match idx {
            FloatType::F16 => &self.fp16,
            FloatType::F32 => &self.fp32,
            FloatType::F64 => &self.fp64,
        }
    }
}

#[derive(Clone, Copy, Eq, Hash, PartialEq)]
enum SyncType {
    Sync,
    Brk,
    Cont,
}

struct ShaderFromNir<'a> {
    nir: &'a nir_shader,
    sm: &'a dyn ShaderModel,
    info: ShaderInfo,
    float_ctl: ShaderFloatControls,
    cfg: CFGBuilder<u32, BasicBlock, FxBuildHasher>,
    label_alloc: LabelAllocator,
    block_label: FxHashMap<u32, Label>,
    bar_label: FxHashMap<u32, Label>,
    sync_blocks: FxHashSet<u32>,
    crs: Vec<(u32, SyncType)>,
    fs_out_regs: [Option<SSAValue>; 34],
    end_block_id: u32,
    ssa_map: FxHashMap<u32, Vec<SSAValue>>,
    saturated: FxHashSet<*const nir_def>,
    nir_instr_printer: NirInstrPrinter,
}

impl<'a> ShaderFromNir<'a> {
    fn new(
        nak: &nak_compiler,
        nir: &'a nir_shader,
        sm: &'a dyn ShaderModel,
    ) -> Self {
        Self {
            nir: nir,
            sm: sm,
            info: init_info_from_nir(nak, nir),
            float_ctl: ShaderFloatControls::from_nir(nir),
            cfg: CFGBuilder::new(),
            label_alloc: LabelAllocator::new(),
            block_label: Default::default(),
            bar_label: Default::default(),
            sync_blocks: Default::default(),
            crs: Vec::new(),
            fs_out_regs: [None; 34],
            end_block_id: 0,
            ssa_map: Default::default(),
            saturated: Default::default(),
            nir_instr_printer: NirInstrPrinter::new().unwrap(),
        }
    }

    fn get_block_label(&mut self, block: &nir_block) -> Label {
        *self
            .block_label
            .entry(block.index)
            .or_insert_with(|| self.label_alloc.alloc())
    }

    fn push_crs(&mut self, target: &nir_block, sync_type: SyncType) {
        self.sync_blocks.insert(target.index);
        self.crs.push((target.index, sync_type));
        let crs_depth = u32::try_from(self.crs.len()).unwrap();
        self.info.max_crs_depth = max(self.info.max_crs_depth, crs_depth);
    }

    fn pop_crs(&mut self, target: &nir_block, sync_type: SyncType) {
        if let Some((top_index, top_sync_type)) = self.crs.pop() {
            assert!(top_index == target.index);
            assert!(top_sync_type == sync_type);
        } else {
            panic!("Tried to pop an empty stack");
        }
    }

    fn peek_crs(&self, target: &nir_block) -> Option<SyncType> {
        for (i, (index, sync_type)) in self.crs.iter().enumerate().rev() {
            if *index != target.index {
                continue;
            }

            match sync_type {
                SyncType::Sync => {
                    // Sync must always be top-of-stack
                    assert!(i == self.crs.len() - 1);
                }
                SyncType::Brk => {
                    // Brk cannot skip over another Brk
                    for (_, inner_sync) in &self.crs[(i + 1)..] {
                        assert!(*inner_sync != SyncType::Brk);
                    }
                }
                SyncType::Cont => {
                    // Cont can only skip over Sync
                    for (_, inner_sync) in &self.crs[(i + 1)..] {
                        assert!(*inner_sync == SyncType::Sync);
                    }
                }
            }

            return Some(*sync_type);
        }

        assert!(!self.sync_blocks.contains(&target.index));
        None
    }

    fn get_ssa(&mut self, ssa: &nir_def) -> &[SSAValue] {
        self.ssa_map.get(&ssa.index).unwrap()
    }

    fn set_ssa(&mut self, def: &nir_def, vec: Vec<SSAValue>) {
        if def.bit_size == 1 {
            for s in &vec {
                assert!(s.is_predicate());
            }
        } else {
            for s in &vec {
                assert!(!s.is_predicate());
            }
            let bits =
                usize::from(def.bit_size) * usize::from(def.num_components);
            assert!(vec.len() == bits.div_ceil(32));
        }
        self.ssa_map
            .entry(def.index)
            .and_modify(|_| panic!("Cannot set an SSA def twice"))
            .or_insert(vec);
    }

    fn get_ssa_comp(&mut self, def: &nir_def, c: u8) -> (SSARef, u8) {
        let vec = self.get_ssa(def);
        match def.bit_size {
            1 => (vec[usize::from(c)].into(), 0),
            8 => (vec[usize::from(c / 4)].into(), c % 4),
            16 => (vec[usize::from(c / 2)].into(), (c * 2) % 4),
            32 => (vec[usize::from(c)].into(), 0),
            64 => {
                let comps =
                    [vec[usize::from(c) * 2 + 0], vec[usize::from(c) * 2 + 1]];
                (comps.into(), 0)
            }
            _ => panic!("Unsupported bit size: {}", def.bit_size),
        }
    }

    fn get_ssa_ref(&mut self, src: &nir_src) -> SSARef {
        SSARef::try_from(self.get_ssa(src.as_def())).unwrap()
    }

    fn get_src(&mut self, src: &nir_src) -> Src {
        self.get_ssa_ref(src).into()
    }

    fn get_io_addr_offset(
        &mut self,
        addr: &nir_src,
        imm_bits: u8,
    ) -> (Src, i32) {
        let addr = addr.as_def();
        let addr_offset = unsafe {
            nak_get_io_addr_offset(addr as *const _ as *mut _, imm_bits)
        };

        if let Some(base_def) = std::ptr::NonNull::new(addr_offset.base.def) {
            let base_def = unsafe { base_def.as_ref() };
            let base_comp = u8::try_from(addr_offset.base.comp).unwrap();
            let (base, _) = self.get_ssa_comp(base_def, base_comp);
            (base.into(), addr_offset.offset)
        } else {
            (SrcRef::Zero.into(), addr_offset.offset)
        }
    }

    fn get_cbuf_addr_offset(&mut self, addr: &nir_src) -> (Src, u16) {
        let (off, off_imm) = self.get_io_addr_offset(addr, 16);
        if let Ok(off_imm_u16) = u16::try_from(off_imm) {
            (off, off_imm_u16)
        } else {
            (self.get_src(addr), 0)
        }
    }

    fn set_dst(&mut self, def: &nir_def, ssa: SSARef) {
        self.set_ssa(def, (*ssa).into());
    }

    fn try_saturate_alu_dst(&mut self, def: &nir_def) -> bool {
        if def.all_uses_are_fsat() {
            self.saturated.insert(def as *const _);
            true
        } else {
            false
        }
    }

    fn alu_src_is_saturated(&self, src: &nir_alu_src) -> bool {
        self.saturated.contains(&(src.src.as_def() as *const _))
    }

    fn parse_alu(&mut self, b: &mut impl SSABuilder, alu: &nir_alu_instr) {
        // Handle vectors and pack ops as a special case since they're the only
        // ALU ops that can produce more than 16B. They are also the only ALU
        // ops which we allow to consume small (8 and 16-bit) vector data
        // scattered across multiple dwords
        match alu.op {
            nir_op_mov
            | nir_op_pack_32_4x8
            | nir_op_pack_32_4x8_split
            | nir_op_pack_32_2x16
            | nir_op_pack_32_2x16_split
            | nir_op_pack_64_2x32
            | nir_op_pack_64_2x32_split
            | nir_op_pack_64_4x16
            | nir_op_vec2
            | nir_op_vec3
            | nir_op_vec4
            | nir_op_vec5
            | nir_op_vec8
            | nir_op_vec16 => {
                let src_bit_size = alu.get_src(0).src.bit_size();
                let bits = usize::from(alu.def.num_components)
                    * usize::from(alu.def.bit_size);

                // Collect the sources into a vec with src_bit_size per SSA
                // value in the vec.  This implicitly makes 64-bit sources look
                // like two 32-bit values
                let mut srcs = Vec::new();
                if alu.info().num_inputs == 1 {
                    let src = alu.get_src(0);
                    for c in 0..alu.src_components(0) {
                        let s = src.swizzle[usize::from(c)];
                        let (src, byte) =
                            self.get_ssa_comp(src.src.as_def(), s);
                        for ssa in src.iter() {
                            srcs.push((*ssa, byte));
                        }
                    }
                } else {
                    for src in alu.srcs_as_slice().iter() {
                        let s = src.swizzle[0];
                        let (src, byte) =
                            self.get_ssa_comp(src.src.as_def(), s);
                        for ssa in src.iter() {
                            srcs.push((*ssa, byte));
                        }
                    }
                }

                let mut comps = Vec::new();
                match src_bit_size {
                    1 | 32 | 64 => {
                        for (ssa, byte) in srcs {
                            assert!(byte == 0);
                            // Always insert a copy regardless of whether or not
                            // we need to permute bytes.  It's possible that the
                            // source is uniform but the destination is not and
                            // we'll need a copy in that case.  If the copy
                            // isn't needed, copy-prop should clean it up for
                            // us.
                            comps.push(b.copy(ssa.into()));
                        }
                    }
                    8 => {
                        for dc in 0..bits.div_ceil(32) {
                            let mut psrc = [None, None, None, None];
                            let mut psel = [0_u8; 4];

                            for b in 0..4 {
                                let sc = dc * 4 + b;
                                if sc < srcs.len() {
                                    let (ssa, byte) = srcs[sc];
                                    // Deduplicate psrc entries
                                    for i in 0..4_u8 {
                                        let psrc_i = &mut psrc[usize::from(i)];
                                        if psrc_i.is_none() {
                                            *psrc_i = Some(ssa.into());
                                        } else if *psrc_i
                                            != Some(Src::from(ssa))
                                        {
                                            continue;
                                        }
                                        psel[b] = i * 4 + byte;
                                        break;
                                    }
                                }
                            }

                            let psrc = {
                                let mut res = [Src::ZERO; 4];

                                for (idx, src) in psrc.into_iter().enumerate() {
                                    if let Some(src) = src {
                                        res[idx] = src;
                                    }
                                }

                                res
                            };

                            comps.push(b.prmt4(psrc, psel));
                        }
                    }
                    16 => {
                        for dc in 0..bits.div_ceil(32) {
                            let mut psrc = [Src::ZERO; 2];
                            let mut psel = [0_u8; 4];

                            for w in 0..2 {
                                let sc = dc * 2 + w;
                                if sc < srcs.len() {
                                    let (ssa, byte) = srcs[sc];
                                    psrc[w] = ssa.into();
                                    psel[w * 2 + 0] = (w as u8 * 4) + byte;
                                    psel[w * 2 + 1] = (w as u8 * 4) + byte + 1;
                                }
                            }
                            let [psrc0, psrc1] = psrc;
                            comps.push(b.prmt(psrc0, psrc1, psel));
                        }
                    }
                    _ => panic!("Unknown bit size: {src_bit_size}"),
                }

                self.set_ssa(&alu.def, comps);
                return;
            }
            _ => (),
        }

        let nir_srcs = alu.srcs_as_slice();
        let mut srcs_vec: Vec<Option<Src>> = Vec::new();
        for (i, alu_src) in nir_srcs.iter().enumerate() {
            let bit_size = alu_src.src.bit_size();
            let comps = alu.src_components(i.try_into().unwrap());
            let ssa = self.get_ssa(alu_src.src.as_def());

            match bit_size {
                1 => {
                    assert!(comps == 1);
                    let s = usize::from(alu_src.swizzle[0]);
                    srcs_vec.push(Some(ssa[s].into()));
                }
                8 | 16 => {
                    let num_bytes = usize::from(comps * (bit_size / 8));
                    assert!(num_bytes <= 4);

                    let mut bytes = [0_u8; 4];
                    for c in 0..usize::from(comps) {
                        let cs = alu_src.swizzle[c];
                        if bit_size == 8 {
                            bytes[c] = cs;
                        } else {
                            bytes[c * 2 + 0] = cs * 2 + 0;
                            bytes[c * 2 + 1] = cs * 2 + 1;
                        }
                    }

                    let mut prmt_srcs = [Src::ZERO; 4];
                    let mut prmt = [0_u8; 4];
                    for b in 0..num_bytes {
                        for (ds, s) in prmt_srcs.iter_mut().enumerate() {
                            let dw = ssa[usize::from(bytes[b] / 4)];
                            if s.is_zero() {
                                *s = dw.into();
                            } else if *s != Src::from(dw) {
                                continue;
                            }
                            prmt[b] = (ds as u8) * 4 + (bytes[b] % 4);
                            break;
                        }
                    }

                    srcs_vec.push(Some(b.prmt4(prmt_srcs, prmt).into()));
                }
                32 => {
                    assert!(comps == 1);
                    let s = usize::from(alu_src.swizzle[0]);
                    srcs_vec.push(Some(ssa[s].into()));
                }
                64 => {
                    assert!(comps == 1);
                    let s = usize::from(alu_src.swizzle[0]);
                    srcs_vec.push(Some([ssa[s * 2], ssa[s * 2 + 1]].into()));
                }
                _ => panic!("Invalid bit size: {bit_size}"),
            }
        }
        let mut srcs =
            |i: usize| -> Src { std::mem::take(&mut srcs_vec[i]).unwrap() };

        // Restricts an F16v2 source to just x if the ALU op is single-component. This
        // must only be called for per-component sources (see nir_op_info::output_sizes
        // for more details).
        let restrict_f16v2_src = |mut src: Src| {
            if alu.def.num_components == 1 {
                src.src_swizzle = SrcSwizzle::Xx;
            }
            src
        };

        let dst: SSARef = match alu.op {
            nir_op_b2b1 => {
                assert!(alu.get_src(0).bit_size() == 32);
                b.isetp(IntCmpType::I32, IntCmpOp::Ne, srcs(0), 0.into())
                    .into()
            }
            nir_op_b2b32 | nir_op_b2i8 | nir_op_b2i16 | nir_op_b2i32 => {
                b.sel(srcs(0).bnot(), 0.into(), 1.into()).into()
            }
            nir_op_b2i64 => {
                let lo = b.sel(srcs(0).bnot(), 0.into(), 1.into());
                let hi = b.copy(0.into());
                [lo, hi].into()
            }
            nir_op_b2f16 => {
                b.sel(srcs(0).bnot(), 0.into(), 0x3c00.into()).into()
            }
            nir_op_b2f32 => {
                b.sel(srcs(0).bnot(), 0.0_f32.into(), 1.0_f32.into()).into()
            }
            nir_op_b2f64 => {
                let lo = b.copy(0.into());
                let hi = b.sel(srcs(0).bnot(), 0.into(), 0x3ff00000.into());
                [lo, hi].into()
            }
            nir_op_bcsel => b.sel(srcs(0), srcs(1), srcs(2)).into(),
            nir_op_bfm => {
                let dst = b.alloc_ssa(RegFile::GPR);
                b.push_op(OpBMsk {
                    dst: dst.into(),
                    pos: srcs(1),
                    width: srcs(0),
                    wrap: true,
                });
                dst.into()
            }
            nir_op_bit_count => {
                let dst = b.alloc_ssa(RegFile::GPR);
                b.push_op(OpPopC {
                    dst: dst.into(),
                    src: srcs(0),
                });
                dst.into()
            }
            nir_op_bitfield_reverse => b.brev(srcs(0)).into(),
            nir_op_ibitfield_extract | nir_op_ubitfield_extract => {
                let range = b.alloc_ssa(RegFile::GPR);
                b.push_op(OpPrmt {
                    dst: range.into(),
                    srcs: [srcs(1), srcs(2)],
                    sel: 0x0040.into(),
                    mode: PrmtMode::Index,
                });

                let dst = b.alloc_ssa(RegFile::GPR);
                b.push_op(OpBfe {
                    dst: dst.into(),
                    base: srcs(0),
                    signed: !matches!(alu.op, nir_op_ubitfield_extract),
                    range: range.into(),
                    reverse: false,
                });
                dst.into()
            }
            nir_op_extract_u8 | nir_op_extract_i8 | nir_op_extract_u16
            | nir_op_extract_i16 => {
                let src1 = alu.get_src(1);
                let elem = src1.src.comp_as_uint(src1.swizzle[0]).unwrap();
                let elem = u8::try_from(elem).unwrap();

                match alu.op {
                    nir_op_extract_u8 => {
                        assert!(elem < 4);
                        let byte = elem;
                        let zero = 4;
                        b.prmt(srcs(0), 0.into(), [byte, zero, zero, zero])
                    }
                    nir_op_extract_i8 => {
                        assert!(elem < 4);
                        let byte = elem;
                        let sign = byte | 0x8;
                        b.prmt(srcs(0), 0.into(), [byte, sign, sign, sign])
                    }
                    nir_op_extract_u16 => {
                        assert!(elem < 2);
                        let byte = elem * 2;
                        let zero = 4;
                        b.prmt(srcs(0), 0.into(), [byte, byte + 1, zero, zero])
                    }
                    nir_op_extract_i16 => {
                        assert!(elem < 2);
                        let byte = elem * 2;
                        let sign = (byte + 1) | 0x8;
                        b.prmt(srcs(0), 0.into(), [byte, byte + 1, sign, sign])
                    }
                    _ => panic!("Unknown extract op: {}", alu.op),
                }
                .into()
            }
            nir_op_f2f16 | nir_op_f2f16_rtne | nir_op_f2f16_rtz
            | nir_op_f2f32 | nir_op_f2f64 => {
                let src_bits = alu.get_src(0).src.bit_size();
                let dst_bits = alu.def.bit_size();
                let src_type = FloatType::from_bits(src_bits.into());
                let dst_type = FloatType::from_bits(dst_bits.into());

                let dst = b.alloc_ssa_vec(RegFile::GPR, dst_bits.div_ceil(32));
                b.push_op(OpF2F {
                    dst: dst.clone().into(),
                    src: srcs(0),
                    src_type: FloatType::from_bits(src_bits.into()),
                    dst_type: dst_type,
                    rnd_mode: match alu.op {
                        nir_op_f2f16_rtne => FRndMode::NearestEven,
                        nir_op_f2f16_rtz => FRndMode::Zero,
                        _ => self.float_ctl[dst_type].rnd_mode,
                    },
                    ftz: if src_bits < dst_bits {
                        self.float_ctl[src_type].ftz
                    } else {
                        self.float_ctl[dst_type].ftz
                    },
                    high: false,
                    integer_rnd: false,
                });
                dst
            }
            nir_op_find_lsb => {
                let rev = b.brev(srcs(0));
                let dst = b.alloc_ssa(RegFile::GPR);
                b.push_op(OpFlo {
                    dst: dst.into(),
                    src: rev.into(),
                    signed: false,
                    return_shift_amount: true,
                });
                dst.into()
            }
            nir_op_f2i8 | nir_op_f2i16 | nir_op_f2i32 | nir_op_f2i64
            | nir_op_f2u8 | nir_op_f2u16 | nir_op_f2u32 | nir_op_f2u64 => {
                let src_bits = usize::from(alu.get_src(0).bit_size());
                let dst_bits = alu.def.bit_size();
                let src_type = FloatType::from_bits(src_bits);
                let dst = b.alloc_ssa_vec(RegFile::GPR, dst_bits.div_ceil(32));
                let dst_is_signed = alu.info().output_type & 2 != 0;
                let dst_type =
                    IntType::from_bits(dst_bits.into(), dst_is_signed);
                if b.sm() < 70 && dst_bits == 8 {
                    // F2I doesn't support 8-bit destinations pre-Volta
                    let tmp = b.alloc_ssa(RegFile::GPR);
                    let tmp_type = IntType::from_bits(32, dst_is_signed);
                    b.push_op(OpF2I {
                        dst: tmp.into(),
                        src: srcs(0),
                        src_type,
                        dst_type: tmp_type,
                        rnd_mode: FRndMode::Zero,
                        ftz: self.float_ctl[src_type].ftz,
                    });
                    b.push_op(OpI2I {
                        dst: dst.clone().into(),
                        src: tmp.into(),
                        src_type: tmp_type,
                        dst_type,
                        saturate: true,
                        abs: false,
                        neg: false,
                    });
                } else {
                    b.push_op(OpF2I {
                        dst: dst.clone().into(),
                        src: srcs(0),
                        src_type,
                        dst_type,
                        rnd_mode: FRndMode::Zero,
                        ftz: self.float_ctl[src_type].ftz,
                    });
                }
                dst
            }
            nir_op_fabs | nir_op_fadd | nir_op_fneg => {
                let (x, y) = match alu.op {
                    nir_op_fabs => (Src::ZERO.fneg(), srcs(0).fabs()),
                    nir_op_fadd => (srcs(0), srcs(1)),
                    nir_op_fneg => (Src::ZERO.fneg(), srcs(0).fneg()),
                    _ => panic!("Unhandled case"),
                };
                let ftype = FloatType::from_bits(alu.def.bit_size().into());
                let dst;
                if alu.def.bit_size() == 64 {
                    dst = b.alloc_ssa_vec(RegFile::GPR, 2);
                    b.push_op(OpDAdd {
                        dst: dst.clone().into(),
                        srcs: [x, y],
                        rnd_mode: self.float_ctl[ftype].rnd_mode,
                    });
                } else if alu.def.bit_size() == 32 {
                    dst = b.alloc_ssa_vec(RegFile::GPR, 1);
                    b.push_op(OpFAdd {
                        dst: dst.clone().into(),
                        srcs: [x, y],
                        saturate: self.try_saturate_alu_dst(&alu.def),
                        rnd_mode: self.float_ctl[ftype].rnd_mode,
                        ftz: self.float_ctl[ftype].ftz,
                    });
                } else if alu.def.bit_size() == 16 {
                    assert!(
                        self.float_ctl[ftype].rnd_mode == FRndMode::NearestEven
                    );

                    dst = b.alloc_ssa_vec(RegFile::GPR, 1);
                    b.push_op(OpHAdd2 {
                        dst: dst.clone().into(),
                        srcs: [restrict_f16v2_src(x), restrict_f16v2_src(y)],
                        saturate: self.try_saturate_alu_dst(&alu.def),
                        ftz: self.float_ctl[ftype].ftz,
                        f32: false,
                    });
                } else {
                    panic!("Unsupported float type: f{}", alu.def.bit_size());
                }
                dst
            }
            nir_op_fceil | nir_op_ffloor | nir_op_fround_even
            | nir_op_ftrunc => {
                let dst = b.alloc_ssa(RegFile::GPR);
                let ty = FloatType::from_bits(alu.def.bit_size().into());
                let rnd_mode = match alu.op {
                    nir_op_fceil => FRndMode::PosInf,
                    nir_op_ffloor => FRndMode::NegInf,
                    nir_op_ftrunc => FRndMode::Zero,
                    nir_op_fround_even => FRndMode::NearestEven,
                    _ => unreachable!(),
                };
                let ftz = self.float_ctl[ty].ftz;
                if b.sm() >= 70 {
                    assert!(
                        alu.def.bit_size() == 32 || alu.def.bit_size() == 16
                    );
                    b.push_op(OpFRnd {
                        dst: dst.into(),
                        src: srcs(0),
                        src_type: ty,
                        dst_type: ty,
                        rnd_mode,
                        ftz,
                    });
                } else {
                    assert!(alu.def.bit_size() == 32);
                    b.push_op(OpF2F {
                        dst: dst.into(),
                        src: srcs(0),
                        src_type: ty,
                        dst_type: ty,
                        rnd_mode,
                        ftz,
                        integer_rnd: true,
                        high: false,
                    });
                }
                dst.into()
            }
            nir_op_fcos => b.fcos(srcs(0)).into(),
            nir_op_feq | nir_op_fge | nir_op_flt | nir_op_fneu => {
                let src_type =
                    FloatType::from_bits(alu.get_src(0).bit_size().into());
                let cmp_op = match alu.op {
                    nir_op_feq => FloatCmpOp::OrdEq,
                    nir_op_fge => FloatCmpOp::OrdGe,
                    nir_op_flt => FloatCmpOp::OrdLt,
                    nir_op_fneu => FloatCmpOp::UnordNe,
                    _ => panic!("Usupported float comparison"),
                };

                let dst =
                    b.alloc_ssa_vec(RegFile::Pred, alu.def.num_components);
                if alu.get_src(0).bit_size() == 64 {
                    assert!(alu.def.num_components == 1);
                    b.push_op(OpDSetP {
                        dst: dst.clone().into(),
                        set_op: PredSetOp::And,
                        cmp_op: cmp_op,
                        srcs: [srcs(0), srcs(1)],
                        accum: SrcRef::True.into(),
                    });
                } else if alu.get_src(0).bit_size() == 32 {
                    assert!(alu.def.num_components == 1);
                    b.push_op(OpFSetP {
                        dst: dst.clone().into(),
                        set_op: PredSetOp::And,
                        cmp_op: cmp_op,
                        srcs: [srcs(0), srcs(1)],
                        accum: SrcRef::True.into(),
                        ftz: self.float_ctl[src_type].ftz,
                    });
                } else if alu.get_src(0).bit_size() == 16 {
                    assert!(
                        alu.def.num_components == 1
                            || alu.def.num_components == 2
                    );

                    let dsts = if alu.def.num_components == 2 {
                        [dst[0].into(), dst[1].into()]
                    } else {
                        [dst[0].into(), Dst::None]
                    };

                    b.push_op(OpHSetP2 {
                        dsts,
                        set_op: PredSetOp::And,
                        cmp_op: cmp_op,
                        srcs: [
                            restrict_f16v2_src(srcs(0)),
                            restrict_f16v2_src(srcs(1)),
                        ],
                        accum: SrcRef::True.into(),
                        ftz: self.float_ctl[src_type].ftz,
                        horizontal: false,
                    });
                } else {
                    panic!(
                        "Unsupported float type: f{}",
                        alu.get_src(0).bit_size()
                    );
                }
                dst
            }
            nir_op_fexp2 => b.fexp2(srcs(0)).into(),
            nir_op_ffma => {
                let ftype = FloatType::from_bits(alu.def.bit_size().into());
                let dst;
                if alu.def.bit_size() == 64 {
                    debug_assert!(!self.float_ctl[ftype].ftz);
                    dst = b.alloc_ssa_vec(RegFile::GPR, 2);
                    b.push_op(OpDFma {
                        dst: dst.clone().into(),
                        srcs: [srcs(0), srcs(1), srcs(2)],
                        rnd_mode: self.float_ctl[ftype].rnd_mode,
                    });
                } else if alu.def.bit_size() == 32 {
                    dst = b.alloc_ssa_vec(RegFile::GPR, 1);
                    b.push_op(OpFFma {
                        dst: dst.clone().into(),
                        srcs: [srcs(0), srcs(1), srcs(2)],
                        saturate: self.try_saturate_alu_dst(&alu.def),
                        rnd_mode: self.float_ctl[ftype].rnd_mode,
                        // The hardware doesn't like FTZ+DNZ and DNZ implies FTZ
                        // anyway so only set one of the two bits.
                        ftz: self.float_ctl[ftype].ftz,
                        dnz: false,
                    });
                } else if alu.def.bit_size() == 16 {
                    assert!(
                        self.float_ctl[ftype].rnd_mode == FRndMode::NearestEven
                    );

                    dst = b.alloc_ssa_vec(RegFile::GPR, 1);
                    b.push_op(OpHFma2 {
                        dst: dst.clone().into(),
                        srcs: [
                            restrict_f16v2_src(srcs(0)),
                            restrict_f16v2_src(srcs(1)),
                            restrict_f16v2_src(srcs(2)),
                        ],
                        saturate: self.try_saturate_alu_dst(&alu.def),
                        ftz: self.float_ctl[ftype].ftz,
                        dnz: false,
                        f32: false,
                    });
                } else {
                    panic!("Unsupported float type: f{}", alu.def.bit_size());
                }
                dst
            }
            nir_op_ffmaz => {
                assert!(alu.def.bit_size() == 32);
                // DNZ implies FTZ so we need FTZ set or this is invalid
                assert!(self.float_ctl.fp32.ftz);
                let dst = b.alloc_ssa(RegFile::GPR);
                b.push_op(OpFFma {
                    dst: dst.into(),
                    srcs: [srcs(0), srcs(1), srcs(2)],
                    saturate: self.try_saturate_alu_dst(&alu.def),
                    rnd_mode: self.float_ctl.fp32.rnd_mode,
                    // The hardware doesn't like FTZ+DNZ and DNZ implies FTZ
                    // anyway so only set one of the two bits.
                    ftz: false,
                    dnz: true,
                });
                dst.into()
            }
            nir_op_flog2 => {
                assert!(alu.def.bit_size() == 32);
                b.mufu(MuFuOp::Log2, srcs(0)).into()
            }
            nir_op_fmax | nir_op_fmin => {
                let dst;
                if alu.def.bit_size() == 64 {
                    dst = b.alloc_ssa_vec(RegFile::GPR, 2);
                    b.push_op(OpDMnMx {
                        dst: dst.clone().into(),
                        srcs: [srcs(0), srcs(1)],
                        min: (alu.op == nir_op_fmin).into(),
                    });
                } else if alu.def.bit_size() == 32 {
                    dst = b.alloc_ssa_vec(RegFile::GPR, 1);
                    b.push_op(OpFMnMx {
                        dst: dst.clone().into(),
                        srcs: [srcs(0), srcs(1)],
                        min: (alu.op == nir_op_fmin).into(),
                        ftz: self.float_ctl.fp32.ftz,
                    });
                } else if alu.def.bit_size() == 16 {
                    dst = b.alloc_ssa_vec(RegFile::GPR, 1);
                    b.push_op(OpHMnMx2 {
                        dst: dst.clone().into(),
                        srcs: [
                            restrict_f16v2_src(srcs(0)),
                            restrict_f16v2_src(srcs(1)),
                        ],
                        min: (alu.op == nir_op_fmin).into(),
                        ftz: self.float_ctl.fp16.ftz,
                    });
                } else {
                    panic!("Unsupported float type: f{}", alu.def.bit_size());
                }
                dst
            }
            nir_op_fmul => {
                let ftype = FloatType::from_bits(alu.def.bit_size().into());
                let dst;
                if alu.def.bit_size() == 64 {
                    debug_assert!(!self.float_ctl[ftype].ftz);
                    dst = b.alloc_ssa_vec(RegFile::GPR, 2);
                    b.push_op(OpDMul {
                        dst: dst.clone().into(),
                        srcs: [srcs(0), srcs(1)],
                        rnd_mode: self.float_ctl[ftype].rnd_mode,
                    });
                } else if alu.def.bit_size() == 32 {
                    dst = b.alloc_ssa_vec(RegFile::GPR, 1);
                    b.push_op(OpFMul {
                        dst: dst.clone().into(),
                        srcs: [srcs(0), srcs(1)],
                        saturate: self.try_saturate_alu_dst(&alu.def),
                        rnd_mode: self.float_ctl[ftype].rnd_mode,
                        ftz: self.float_ctl[ftype].ftz,
                        dnz: false,
                    });
                } else if alu.def.bit_size() == 16 {
                    assert!(
                        self.float_ctl[ftype].rnd_mode == FRndMode::NearestEven
                    );

                    dst = b.alloc_ssa_vec(RegFile::GPR, 1);
                    b.push_op(OpHMul2 {
                        dst: dst.clone().into(),
                        srcs: [
                            restrict_f16v2_src(srcs(0)),
                            restrict_f16v2_src(srcs(1)),
                        ],
                        saturate: self.try_saturate_alu_dst(&alu.def),
                        ftz: self.float_ctl[ftype].ftz,
                        dnz: false,
                    });
                } else {
                    panic!("Unsupported float type: f{}", alu.def.bit_size());
                }
                dst
            }
            nir_op_fmulz => {
                assert!(alu.def.bit_size() == 32);
                // DNZ implies FTZ so we need FTZ set or this is invalid
                assert!(self.float_ctl.fp32.ftz);
                let dst = b.alloc_ssa(RegFile::GPR);
                b.push_op(OpFMul {
                    dst: dst.into(),
                    srcs: [srcs(0), srcs(1)],
                    saturate: self.try_saturate_alu_dst(&alu.def),
                    rnd_mode: self.float_ctl.fp32.rnd_mode,
                    // The hardware doesn't like FTZ+DNZ and DNZ implies FTZ
                    // anyway so only set one of the two bits.
                    ftz: false,
                    dnz: true,
                });
                dst.into()
            }
            nir_op_fquantize2f16 => {
                let src0 = srcs(0);
                let tmp = b.alloc_ssa(RegFile::GPR);
                b.push_op(OpF2F {
                    dst: tmp.into(),
                    src: src0.clone(),
                    src_type: FloatType::F32,
                    dst_type: FloatType::F16,
                    rnd_mode: FRndMode::NearestEven,
                    ftz: true,
                    high: false,
                    integer_rnd: false,
                });
                assert!(alu.def.bit_size() == 32);
                let dst = b.alloc_ssa(RegFile::GPR);
                b.push_op(OpF2F {
                    dst: dst.into(),
                    src: tmp.into(),
                    src_type: FloatType::F16,
                    dst_type: FloatType::F32,
                    rnd_mode: FRndMode::NearestEven,
                    ftz: true,
                    high: false,
                    integer_rnd: false,
                });
                if b.sm() < 70 {
                    // Pre-Volta, F2F.ftz doesn't flush denorms so we need to do
                    // that manually
                    let denorm = b.fsetp(
                        FloatCmpOp::OrdLt,
                        src0.clone().fabs(),
                        0x38800000.into(),
                    );
                    // Get the correctly signed zero
                    let zero = b.lop2(LogicOp2::And, src0, 0x80000000.into());
                    b.sel(denorm.into(), zero.into(), dst.into())
                } else {
                    dst
                }
                .into()
            }
            nir_op_frcp => {
                assert!(alu.def.bit_size() == 32);
                b.mufu(MuFuOp::Rcp, srcs(0)).into()
            }
            nir_op_frsq => {
                assert!(alu.def.bit_size() == 32);
                b.mufu(MuFuOp::Rsq, srcs(0)).into()
            }
            nir_op_fsat => {
                let ftype = FloatType::from_bits(alu.def.bit_size().into());

                if self.alu_src_is_saturated(&alu.srcs_as_slice()[0]) {
                    b.copy(srcs(0)).into()
                } else if alu.def.bit_size() == 32 {
                    let dst = b.alloc_ssa(RegFile::GPR);
                    b.push_op(OpFAdd {
                        dst: dst.into(),
                        srcs: [srcs(0), 0.into()],
                        saturate: true,
                        rnd_mode: self.float_ctl[ftype].rnd_mode,
                        ftz: self.float_ctl[ftype].ftz,
                    });
                    dst.into()
                } else if alu.def.bit_size() == 16 {
                    assert!(
                        self.float_ctl[ftype].rnd_mode == FRndMode::NearestEven
                    );

                    let dst = b.alloc_ssa(RegFile::GPR);
                    b.push_op(OpHAdd2 {
                        dst: dst.into(),
                        srcs: [restrict_f16v2_src(srcs(0)), 0.into()],
                        saturate: true,
                        ftz: self.float_ctl[ftype].ftz,
                        f32: false,
                    });
                    dst.into()
                } else {
                    panic!("Unsupported float type: f{}", alu.def.bit_size());
                }
            }
            nir_op_fsign => {
                let src0 = srcs(0);
                if alu.def.bit_size() == 64 {
                    let lz = b.dsetp(FloatCmpOp::OrdLt, src0.clone(), 0.into());
                    let gz = b.dsetp(FloatCmpOp::OrdGt, src0, 0.into());
                    let hi = b.sel(lz.into(), 0xbff00000.into(), 0.into());
                    let hi = b.sel(gz.into(), 0x3ff00000.into(), hi.into());
                    let lo = b.copy(0.into());
                    [lo, hi].into()
                } else if alu.def.bit_size() == 32 {
                    let lz = b.fset(FloatCmpOp::OrdLt, src0.clone(), 0.into());
                    let gz = b.fset(FloatCmpOp::OrdGt, src0, 0.into());
                    b.fadd(gz.into(), Src::from(lz).fneg()).into()
                } else if alu.def.bit_size() == 16 {
                    let x = restrict_f16v2_src(src0);

                    let lz = restrict_f16v2_src(
                        b.hset2(FloatCmpOp::OrdLt, x.clone(), 0.into()).into(),
                    );
                    let gz = restrict_f16v2_src(
                        b.hset2(FloatCmpOp::OrdGt, x, 0.into()).into(),
                    );

                    b.hadd2(gz, lz.fneg()).into()
                } else {
                    panic!("Unsupported float type: f{}", alu.def.bit_size());
                }
            }
            nir_op_fsin => b.fsin(srcs(0)).into(),
            nir_op_fsqrt => b.mufu(MuFuOp::Sqrt, srcs(0)).into(),
            nir_op_i2f16 | nir_op_i2f32 | nir_op_i2f64 => {
                let src_bits = alu.get_src(0).src.bit_size();
                let dst_bits = alu.def.bit_size();
                let dst_type = FloatType::from_bits(dst_bits.into());
                let dst = b.alloc_ssa_vec(RegFile::GPR, dst_bits.div_ceil(32));
                b.push_op(OpI2F {
                    dst: dst.clone().into(),
                    src: srcs(0),
                    dst_type: dst_type,
                    src_type: IntType::from_bits(src_bits.into(), true),
                    rnd_mode: self.float_ctl[dst_type].rnd_mode,
                });
                dst
            }
            nir_op_i2i8 | nir_op_i2i16 | nir_op_i2i32 | nir_op_i2i64
            | nir_op_u2u8 | nir_op_u2u16 | nir_op_u2u32 | nir_op_u2u64 => {
                let src_bits = alu.get_src(0).src.bit_size();
                let dst_bits = alu.def.bit_size();

                let mut prmt = [0_u8; 8];
                match alu.op {
                    nir_op_i2i8 | nir_op_i2i16 | nir_op_i2i32
                    | nir_op_i2i64 => {
                        let sign = ((src_bits / 8) - 1) | 0x8;
                        for i in 0..8 {
                            if i < (src_bits / 8) {
                                prmt[usize::from(i)] = i;
                            } else {
                                prmt[usize::from(i)] = sign;
                            }
                        }
                    }
                    nir_op_u2u8 | nir_op_u2u16 | nir_op_u2u32
                    | nir_op_u2u64 => {
                        for i in 0..8 {
                            if i < (src_bits / 8) {
                                prmt[usize::from(i)] = i;
                            } else {
                                prmt[usize::from(i)] = 4;
                            }
                        }
                    }
                    _ => panic!("Invalid integer conversion: {}", alu.op),
                }
                let prmt_lo: [u8; 4] = prmt[0..4].try_into().unwrap();
                let prmt_hi: [u8; 4] = prmt[4..8].try_into().unwrap();

                let src = srcs(0).to_ssa();
                if src_bits == 64 {
                    if dst_bits == 64 {
                        src.into()
                    } else {
                        b.prmt(src[0].into(), src[1].into(), prmt_lo).into()
                    }
                } else {
                    if dst_bits == 64 {
                        let lo = b.prmt(src[0].into(), 0.into(), prmt_lo);
                        let hi = b.prmt(src[0].into(), 0.into(), prmt_hi);
                        [lo, hi].into()
                    } else {
                        b.prmt(src[0].into(), 0.into(), prmt_lo).into()
                    }
                }
            }
            nir_op_iabs => b.iabs(srcs(0)).into(),
            nir_op_iadd => match alu.def.bit_size {
                32 => b.iadd(srcs(0), srcs(1), 0.into()).into(),
                64 => b.iadd64(srcs(0), srcs(1), 0.into()),
                x => panic!("unsupported bit size for nir_op_iadd: {x}"),
            },
            nir_op_iadd3 => match alu.def.bit_size {
                32 => b.iadd(srcs(0), srcs(1), srcs(2)).into(),
                64 => b.iadd64(srcs(0), srcs(1), srcs(2)),
                x => panic!("unsupported bit size for nir_op_iadd3: {x}"),
            },
            nir_op_iand => b.lop2(LogicOp2::And, srcs(0), srcs(1)).into(),
            nir_op_ieq => {
                if alu.get_src(0).bit_size() == 1 {
                    b.lop2(LogicOp2::Xor, srcs(0), srcs(1).bnot()).into()
                } else if alu.get_src(0).bit_size() == 64 {
                    b.isetp64(IntCmpType::I32, IntCmpOp::Eq, srcs(0), srcs(1))
                } else {
                    assert!(alu.get_src(0).bit_size() == 32);
                    b.isetp(IntCmpType::I32, IntCmpOp::Eq, srcs(0), srcs(1))
                        .into()
                }
            }
            nir_op_ifind_msb | nir_op_ifind_msb_rev | nir_op_ufind_msb
            | nir_op_ufind_msb_rev => {
                let dst = b.alloc_ssa(RegFile::GPR);
                b.push_op(OpFlo {
                    dst: dst.into(),
                    src: srcs(0),
                    signed: match alu.op {
                        nir_op_ifind_msb | nir_op_ifind_msb_rev => true,
                        nir_op_ufind_msb | nir_op_ufind_msb_rev => false,
                        _ => panic!("Not a find_msb op"),
                    },
                    return_shift_amount: match alu.op {
                        nir_op_ifind_msb | nir_op_ufind_msb => false,
                        nir_op_ifind_msb_rev | nir_op_ufind_msb_rev => true,
                        _ => panic!("Not a find_msb op"),
                    },
                });
                dst.into()
            }
            nir_op_ige | nir_op_ilt | nir_op_uge | nir_op_ult => {
                let x = srcs(0).to_ssa();
                let y = srcs(1).to_ssa();
                let (cmp_type, cmp_op) = match alu.op {
                    nir_op_ige => (IntCmpType::I32, IntCmpOp::Ge),
                    nir_op_ilt => (IntCmpType::I32, IntCmpOp::Lt),
                    nir_op_uge => (IntCmpType::U32, IntCmpOp::Ge),
                    nir_op_ult => (IntCmpType::U32, IntCmpOp::Lt),
                    _ => panic!("Not an integer comparison"),
                };
                if alu.get_src(0).bit_size() == 64 {
                    b.isetp64(cmp_type, cmp_op, x.into(), y.into())
                } else {
                    assert!(alu.get_src(0).bit_size() == 32);
                    b.isetp(cmp_type, cmp_op, x.into(), y.into()).into()
                }
            }
            nir_op_imad => {
                assert!(alu.def.bit_size() == 32);
                let dst = b.alloc_ssa(RegFile::GPR);
                b.push_op(OpIMad {
                    dst: dst.into(),
                    srcs: [srcs(0), srcs(1), srcs(2)],
                    signed: false,
                });
                dst.into()
            }
            nir_op_imax | nir_op_imin | nir_op_umax | nir_op_umin => {
                let (tp, min) = match alu.op {
                    nir_op_imax => (IntCmpType::I32, SrcRef::False),
                    nir_op_imin => (IntCmpType::I32, SrcRef::True),
                    nir_op_umax => (IntCmpType::U32, SrcRef::False),
                    nir_op_umin => (IntCmpType::U32, SrcRef::True),
                    _ => panic!("Not an integer min/max"),
                };
                assert!(alu.def.bit_size() == 32);
                b.imnmx(tp, srcs(0), srcs(1), min.into()).into()
            }
            nir_op_imul => {
                assert!(alu.def.bit_size() == 32);
                b.imul(srcs(0), srcs(1)).into()
            }
            nir_op_imul_2x32_64 | nir_op_umul_2x32_64 => {
                let signed = alu.op == nir_op_imul_2x32_64;
                b.imul_2x32_64(srcs(0), srcs(1), signed)
            }
            nir_op_imul_high | nir_op_umul_high => {
                let signed = alu.op == nir_op_imul_high;
                let dst64 = b.imul_2x32_64(srcs(0), srcs(1), signed);
                dst64[1].into()
            }
            nir_op_ine => {
                if alu.get_src(0).bit_size() == 1 {
                    b.lop2(LogicOp2::Xor, srcs(0), srcs(1)).into()
                } else if alu.get_src(0).bit_size() == 64 {
                    b.isetp64(IntCmpType::I32, IntCmpOp::Ne, srcs(0), srcs(1))
                } else {
                    assert!(alu.get_src(0).bit_size() == 32);
                    b.isetp(IntCmpType::I32, IntCmpOp::Ne, srcs(0), srcs(1))
                        .into()
                }
            }
            nir_op_ineg => {
                if alu.def.bit_size == 64 {
                    b.ineg64(srcs(0))
                } else {
                    assert!(alu.def.bit_size() == 32);
                    b.ineg(srcs(0)).into()
                }
            }
            nir_op_inot => {
                if alu.def.bit_size() == 1 {
                    b.lop2(LogicOp2::PassB, true.into(), srcs(0).bnot()).into()
                } else {
                    assert!(alu.def.bit_size() == 32);
                    b.lop2(LogicOp2::PassB, 0.into(), srcs(0).bnot()).into()
                }
            }
            nir_op_ior => b.lop2(LogicOp2::Or, srcs(0), srcs(1)).into(),
            nir_op_ishl => {
                if alu.def.bit_size() == 64 {
                    let shift = if let Some(s) = nir_srcs[1].comp_as_uint(0) {
                        (s as u32).into()
                    } else {
                        srcs(1)
                    };
                    b.shl64(srcs(0), shift)
                } else {
                    assert!(alu.def.bit_size() == 32);
                    b.shl(srcs(0), srcs(1)).into()
                }
            }
            nir_op_ishr => {
                if alu.def.bit_size() == 64 {
                    let shift = if let Some(s) = nir_srcs[1].comp_as_uint(0) {
                        (s as u32).into()
                    } else {
                        srcs(1)
                    };
                    b.shr64(srcs(0), shift, true)
                } else {
                    assert!(alu.def.bit_size() == 32);
                    b.shr(srcs(0), srcs(1), true).into()
                }
            }
            nir_op_lea_nv => {
                let src_a = srcs(1);
                let src_b = srcs(0);
                let shift = nir_srcs[2].comp_as_uint(0).unwrap() as u8;
                match alu.def.bit_size {
                    32 => b.lea(src_a, src_b, shift).into(),
                    64 => b.lea64(src_a, src_b, shift),
                    x => panic!("unsupported bit size for nir_op_lea_nv: {x}"),
                }
            }
            nir_op_isub => match alu.def.bit_size {
                32 => b.iadd(srcs(0), srcs(1).ineg(), 0.into()).into(),
                64 => b.iadd64(srcs(0), srcs(1).ineg(), 0.into()),
                x => panic!("unsupported bit size for nir_op_iadd: {x}"),
            },
            nir_op_ixor => b.lop2(LogicOp2::Xor, srcs(0), srcs(1)).into(),
            nir_op_pack_half_2x16_split | nir_op_pack_half_2x16_rtz_split => {
                assert!(alu.get_src(0).bit_size() == 32);

                let rnd_mode = match alu.op {
                    nir_op_pack_half_2x16_split => FRndMode::NearestEven,
                    nir_op_pack_half_2x16_rtz_split => FRndMode::Zero,
                    _ => panic!("Unhandled fp16 pack op"),
                };

                if self.sm.sm() >= 86 {
                    let result = b.alloc_ssa(RegFile::GPR);
                    b.push_op(OpF2FP {
                        dst: result.into(),
                        srcs: [srcs(1), srcs(0)],
                        rnd_mode: rnd_mode,
                    });

                    result.into()
                } else {
                    let low = b.alloc_ssa(RegFile::GPR);
                    let high = b.alloc_ssa(RegFile::GPR);

                    b.push_op(OpF2F {
                        dst: low.into(),
                        src: srcs(0),
                        src_type: FloatType::F32,
                        dst_type: FloatType::F16,
                        rnd_mode: rnd_mode,
                        ftz: false,
                        high: false,
                        integer_rnd: false,
                    });

                    let src_bits = usize::from(alu.get_src(1).bit_size());
                    let src_type = FloatType::from_bits(src_bits);
                    assert!(matches!(src_type, FloatType::F32));
                    b.push_op(OpF2F {
                        dst: high.into(),
                        src: srcs(1),
                        src_type: FloatType::F32,
                        dst_type: FloatType::F16,
                        rnd_mode: rnd_mode,
                        ftz: false,
                        high: false,
                        integer_rnd: false,
                    });

                    b.prmt(low.into(), high.into(), [0, 1, 4, 5]).into()
                }
            }
            nir_op_prmt_nv => {
                let dst = b.alloc_ssa(RegFile::GPR);
                b.push_op(OpPrmt {
                    dst: dst.into(),
                    srcs: [srcs(1), srcs(2)],
                    sel: srcs(0),
                    mode: PrmtMode::Index,
                });
                dst.into()
            }
            nir_op_sdot_4x8_iadd => {
                let dst = b.alloc_ssa(RegFile::GPR);
                b.push_op(OpIDp4 {
                    dst: dst.into(),
                    src_types: [IntType::I8, IntType::I8],
                    srcs: [srcs(0), srcs(1), srcs(2)],
                });
                dst.into()
            }
            nir_op_sudot_4x8_iadd => {
                let dst = b.alloc_ssa(RegFile::GPR);
                b.push_op(OpIDp4 {
                    dst: dst.into(),
                    src_types: [IntType::I8, IntType::U8],
                    srcs: [srcs(0), srcs(1), srcs(2)],
                });
                dst.into()
            }
            nir_op_udot_4x8_uadd => {
                let dst = b.alloc_ssa(RegFile::GPR);
                b.push_op(OpIDp4 {
                    dst: dst.into(),
                    src_types: [IntType::U8, IntType::U8],
                    srcs: [srcs(0), srcs(1), srcs(2)],
                });
                dst.into()
            }
            nir_op_u2f16 | nir_op_u2f32 | nir_op_u2f64 => {
                let src_bits = alu.get_src(0).src.bit_size();
                let dst_bits = alu.def.bit_size();
                let dst_type = FloatType::from_bits(dst_bits.into());
                let dst = b.alloc_ssa_vec(RegFile::GPR, dst_bits.div_ceil(32));
                b.push_op(OpI2F {
                    dst: dst.clone().into(),
                    src: srcs(0),
                    dst_type: dst_type,
                    src_type: IntType::from_bits(src_bits.into(), false),
                    rnd_mode: self.float_ctl[dst_type].rnd_mode,
                });
                dst
            }
            nir_op_uadd_sat => {
                let x = srcs(0).to_ssa();
                let y = srcs(1).to_ssa();
                let sum_lo = b.alloc_ssa(RegFile::GPR);
                let ovf_lo = b.alloc_ssa(RegFile::Pred);
                b.push_op(OpIAdd3 {
                    dst: sum_lo.into(),
                    overflow: [ovf_lo.into(), Dst::None],
                    srcs: [0.into(), x[0].into(), y[0].into()],
                });
                if alu.def.bit_size() == 64 {
                    let sum_hi = b.alloc_ssa(RegFile::GPR);
                    let ovf_hi = b.alloc_ssa(RegFile::Pred);
                    b.push_op(OpIAdd3X {
                        dst: sum_hi.into(),
                        overflow: [ovf_hi.into(), Dst::None],
                        srcs: [0.into(), x[1].into(), y[1].into()],
                        carry: [ovf_lo.into(), false.into()],
                    });
                    let lo =
                        b.sel(ovf_hi.into(), u32::MAX.into(), sum_lo.into());
                    let hi =
                        b.sel(ovf_hi.into(), u32::MAX.into(), sum_hi.into());
                    [lo, hi].into()
                } else {
                    assert!(alu.def.bit_size() == 32);
                    b.sel(ovf_lo.into(), u32::MAX.into(), sum_lo.into()).into()
                }
            }
            nir_op_usub_sat => {
                let x = srcs(0).to_ssa();
                let y = srcs(1).to_ssa();
                let sum_lo = b.alloc_ssa(RegFile::GPR);
                let ovf_lo = b.alloc_ssa(RegFile::Pred);
                // The result of OpIAdd3X is the 33-bit value
                //
                //  s|o = x + !y + 1
                //
                // The overflow bit of this result is true if and only if the
                // subtract did NOT overflow.
                b.push_op(OpIAdd3 {
                    dst: sum_lo.into(),
                    overflow: [ovf_lo.into(), Dst::None],
                    srcs: [0.into(), x[0].into(), Src::from(y[0]).ineg()],
                });
                if alu.def.bit_size() == 64 {
                    let sum_hi = b.alloc_ssa(RegFile::GPR);
                    let ovf_hi = b.alloc_ssa(RegFile::Pred);
                    b.push_op(OpIAdd3X {
                        dst: sum_hi.into(),
                        overflow: [ovf_hi.into(), Dst::None],
                        srcs: [0.into(), x[1].into(), Src::from(y[1]).bnot()],
                        carry: [ovf_lo.into(), false.into()],
                    });
                    let lo = b.sel(ovf_hi.into(), sum_lo.into(), 0.into());
                    let hi = b.sel(ovf_hi.into(), sum_hi.into(), 0.into());
                    [lo, hi].into()
                } else {
                    assert!(alu.def.bit_size() == 32);
                    b.sel(ovf_lo.into(), sum_lo.into(), 0.into()).into()
                }
            }
            nir_op_unpack_32_2x16 | nir_op_unpack_32_4x8 => {
                b.copy(srcs(0)).into()
            }
            nir_op_unpack_32_2x16_split_x => {
                b.prmt(srcs(0), 0.into(), [0, 1, 4, 4]).into()
            }
            nir_op_unpack_32_2x16_split_y => {
                b.prmt(srcs(0), 0.into(), [2, 3, 4, 4]).into()
            }
            nir_op_unpack_64_2x32 | nir_op_unpack_64_4x16 => {
                let src0 = srcs(0).to_ssa();
                [b.copy(src0[0].into()), b.copy(src0[1].into())].into()
            }
            nir_op_unpack_64_2x32_split_x => {
                let src0_x = srcs(0).as_ssa().unwrap()[0];
                b.copy(src0_x.into()).into()
            }
            nir_op_unpack_64_2x32_split_y => {
                let src0_y = srcs(0).as_ssa().unwrap()[1];
                b.copy(src0_y.into()).into()
            }
            nir_op_unpack_half_2x16_split_x
            | nir_op_unpack_half_2x16_split_y => {
                assert!(alu.def.bit_size() == 32);
                let dst = b.alloc_ssa(RegFile::GPR);

                b.push_op(OpF2F {
                    dst: dst.into(),
                    src: srcs(0),
                    src_type: FloatType::F16,
                    dst_type: FloatType::F32,
                    rnd_mode: FRndMode::NearestEven,
                    ftz: false,
                    high: alu.op == nir_op_unpack_half_2x16_split_y,
                    integer_rnd: false,
                });

                dst.into()
            }
            nir_op_ushr => {
                if alu.def.bit_size() == 64 {
                    let shift = if let Some(s) = nir_srcs[1].comp_as_uint(0) {
                        (s as u32).into()
                    } else {
                        srcs(1)
                    };
                    b.shr64(srcs(0), shift, false)
                } else {
                    assert!(alu.def.bit_size() == 32);
                    b.shr(srcs(0), srcs(1), false).into()
                }
            }
            _ => panic!("Unsupported ALU instruction: {}", alu.info().name()),
        };
        self.set_dst(&alu.def, dst);
    }

    fn parse_tex(&mut self, b: &mut impl SSABuilder, tex: &nir_tex_instr) {
        let dim = match tex.sampler_dim {
            GLSL_SAMPLER_DIM_1D => {
                if tex.is_array {
                    TexDim::Array1D
                } else {
                    TexDim::_1D
                }
            }
            GLSL_SAMPLER_DIM_2D => {
                if tex.is_array {
                    TexDim::Array2D
                } else {
                    TexDim::_2D
                }
            }
            GLSL_SAMPLER_DIM_3D => {
                assert!(!tex.is_array);
                TexDim::_3D
            }
            GLSL_SAMPLER_DIM_CUBE => {
                if tex.is_array {
                    TexDim::ArrayCube
                } else {
                    TexDim::Cube
                }
            }
            GLSL_SAMPLER_DIM_BUF => TexDim::_1D,
            GLSL_SAMPLER_DIM_MS => {
                if tex.is_array {
                    TexDim::Array2D
                } else {
                    TexDim::_2D
                }
            }
            _ => panic!("Unsupported texture dimension: {}", tex.sampler_dim),
        };

        let srcs = tex.srcs_as_slice();
        assert!(srcs[0].src_type == nir_tex_src_backend1);
        if srcs.len() > 1 {
            assert!(srcs.len() == 2);
            assert!(srcs[1].src_type == nir_tex_src_backend2);
        }

        let flags: nak_nir_tex_flags =
            unsafe { std::mem::transmute_copy(&tex.backend_flags) };

        let tex_ref = match flags.ref_type() {
            NAK_NIR_TEX_REF_TYPE_BOUND => {
                TexRef::Bound(tex.texture_index.try_into().unwrap())
            }
            NAK_NIR_TEX_REF_TYPE_CBUF => TexRef::CBuf(TexCBufRef {
                idx: (tex.texture_index >> 16).try_into().unwrap(),
                offset: tex.texture_index as u16,
            }),
            NAK_NIR_TEX_REF_TYPE_BINDLESS => TexRef::Bindless,
            _ => panic!("Invalid tex ref type"),
        };

        let mask = tex.def.components_read();
        let mut mask = u8::try_from(mask).unwrap();
        if flags.is_sparse() {
            mask &= !(1 << (tex.def.num_components - 1));
            if mask == 0 {
                // This can happen if only the sparse predicate is used.  In
                // that case, we need at least one result register.
                mask = 1;
            }
        } else {
            debug_assert!(mask != 0);
        }
        let channel_mask = ChannelMask::new(mask);

        let dst_comps = u8::try_from(mask.count_ones()).unwrap();
        let dst = b.alloc_ssa_vec(RegFile::GPR, dst_comps);

        // On Volta and later, the destination is split in two
        let mut dsts = [Dst::None, Dst::None];
        if dst_comps > 2 && b.sm() >= 70 {
            dsts[0] = SSARef::try_from(&dst[0..2]).unwrap().into();
            dsts[1] = SSARef::try_from(&dst[2..]).unwrap().into();
        } else {
            dsts[0] = dst.clone().into();
        }

        let fault = if flags.is_sparse() {
            Some(b.alloc_ssa(RegFile::Pred))
        } else {
            None
        };

        if tex.op == nir_texop_hdr_dim_nv {
            let src = self.get_src(&srcs[0].src);
            assert!(fault.is_none());
            b.push_op(OpTxq {
                dsts: dsts,
                tex: tex_ref,
                src: src,
                query: TexQuery::Dimension,
                nodep: flags.nodep(),
                channel_mask,
            });
        } else if tex.op == nir_texop_tex_type_nv {
            let src = self.get_src(&srcs[0].src);
            assert!(fault.is_none());
            b.push_op(OpTxq {
                dsts: dsts,
                tex: tex_ref,
                src: src,
                query: TexQuery::TextureType,
                nodep: flags.nodep(),
                channel_mask,
            });
        } else if tex.op == nir_texop_sample_pos_nv {
            let src = self.get_src(&srcs[0].src);
            assert!(fault.is_none());
            b.push_op(OpTxq {
                dsts: dsts,
                tex: tex_ref,
                src: src,
                query: TexQuery::SamplerPos,
                nodep: flags.nodep(),
                channel_mask,
            });
        } else {
            let lod_mode = match flags.lod_mode() {
                NAK_NIR_LOD_MODE_AUTO => TexLodMode::Auto,
                NAK_NIR_LOD_MODE_ZERO => TexLodMode::Zero,
                NAK_NIR_LOD_MODE_BIAS => TexLodMode::Bias,
                NAK_NIR_LOD_MODE_LOD => TexLodMode::Lod,
                NAK_NIR_LOD_MODE_CLAMP => TexLodMode::Clamp,
                NAK_NIR_LOD_MODE_BIAS_CLAMP => TexLodMode::BiasClamp,
                _ => panic!("Invalid LOD mode"),
            };

            // Starting with Blackwell B, the shader stage check for derivatives
            // is back to defaulting to disabled on compute and instead we have
            // a new derivative mode to re-enable it.  If tex_lod_mode == Zero,
            // there is no implicit derivative so this doesn't matter.
            let deriv_mode =
                if self.sm.sm() >= 120 && lod_mode != TexLodMode::Zero {
                    TexDerivMode::DerivXY
                } else {
                    TexDerivMode::Auto
                };

            let offset_mode = match flags.offset_mode() {
                NAK_NIR_OFFSET_MODE_NONE => TexOffsetMode::None,
                NAK_NIR_OFFSET_MODE_AOFFI => TexOffsetMode::AddOffI,
                NAK_NIR_OFFSET_MODE_PER_PX => TexOffsetMode::PerPx,
                _ => panic!("Invalid offset mode"),
            };

            let src0 = self.get_src(&srcs[0].src);
            let src1 = if srcs.len() > 1 {
                self.get_src(&srcs[1].src)
            } else {
                SrcRef::Zero.into()
            };
            let srcs = [src0, src1];

            if tex.op == nir_texop_txd {
                assert!(lod_mode == TexLodMode::Auto);
                assert!(offset_mode != TexOffsetMode::PerPx);
                assert!(!flags.has_z_cmpr());
                b.push_op(OpTxd {
                    dsts: dsts,
                    fault: fault.into(),
                    tex: tex_ref,
                    srcs: srcs,
                    dim: dim,
                    offset_mode,
                    mem_eviction_priority: MemEvictionPriority::Normal,
                    nodep: flags.nodep(),
                    channel_mask,
                });
            } else if tex.op == nir_texop_lod {
                assert!(lod_mode == TexLodMode::Auto);
                assert!(offset_mode == TexOffsetMode::None);
                b.push_op(OpTmml {
                    dsts: dsts,
                    tex: tex_ref,
                    srcs: srcs,
                    dim: dim,
                    deriv_mode,
                    nodep: flags.nodep(),
                    channel_mask,
                });
            } else if tex.op == nir_texop_txf || tex.op == nir_texop_txf_ms {
                assert!(offset_mode != TexOffsetMode::PerPx);
                b.push_op(OpTld {
                    dsts: dsts,
                    fault: fault.into(),
                    tex: tex_ref,
                    srcs: srcs,
                    dim: dim,
                    lod_mode: lod_mode,
                    is_ms: tex.op == nir_texop_txf_ms,
                    offset_mode,
                    mem_eviction_priority: MemEvictionPriority::Normal,
                    nodep: flags.nodep(),
                    channel_mask,
                });
            } else if tex.op == nir_texop_tg4 {
                b.push_op(OpTld4 {
                    dsts: dsts,
                    fault: fault.into(),
                    tex: tex_ref,
                    srcs: srcs,
                    dim: dim,
                    comp: tex.component().try_into().unwrap(),
                    offset_mode: offset_mode,
                    z_cmpr: flags.has_z_cmpr(),
                    mem_eviction_priority: MemEvictionPriority::Normal,
                    nodep: flags.nodep(),
                    channel_mask,
                });
            } else {
                assert!(offset_mode != TexOffsetMode::PerPx);
                b.push_op(OpTex {
                    dsts: dsts,
                    fault: fault.into(),
                    tex: tex_ref,
                    srcs: srcs,
                    dim: dim,
                    lod_mode: lod_mode,
                    deriv_mode,
                    z_cmpr: flags.has_z_cmpr(),
                    offset_mode,
                    mem_eviction_priority: MemEvictionPriority::Normal,
                    nodep: flags.nodep(),
                    channel_mask,
                });
            }
        }

        let mut di = 0_usize;
        let mut nir_dst = Vec::new();
        for i in 0..tex.def.num_components() {
            if flags.is_sparse() && i == tex.def.num_components - 1 {
                let Some(fault) = fault else {
                    panic!("No fault value for sparse op");
                };
                nir_dst.push(b.sel(fault.into(), 0.into(), 1.into()));
            } else if mask & (1 << i) == 0 {
                nir_dst.push(b.copy(0.into()));
            } else {
                nir_dst.push(dst[di]);
                di += 1;
            }
        }

        self.set_ssa(tex.def.as_def(), nir_dst);
    }

    fn get_atomic_type(&self, intrin: &nir_intrinsic_instr) -> AtomType {
        let bit_size = intrin.def.bit_size();
        match intrin.atomic_op() {
            nir_atomic_op_iadd => AtomType::U(bit_size),
            nir_atomic_op_imin => AtomType::I(bit_size),
            nir_atomic_op_umin => AtomType::U(bit_size),
            nir_atomic_op_imax => AtomType::I(bit_size),
            nir_atomic_op_umax => AtomType::U(bit_size),
            nir_atomic_op_iand => AtomType::U(bit_size),
            nir_atomic_op_ior => AtomType::U(bit_size),
            nir_atomic_op_ixor => AtomType::U(bit_size),
            nir_atomic_op_xchg => AtomType::U(bit_size),
            nir_atomic_op_fadd => AtomType::F(bit_size),
            nir_atomic_op_fmin => AtomType::F(bit_size),
            nir_atomic_op_fmax => AtomType::F(bit_size),
            nir_atomic_op_cmpxchg => AtomType::U(bit_size),
            _ => panic!("Unsupported NIR atomic op"),
        }
    }

    fn get_atomic_op(
        &self,
        intrin: &nir_intrinsic_instr,
        cmp_src: AtomCmpSrc,
    ) -> AtomOp {
        match intrin.atomic_op() {
            nir_atomic_op_iadd => AtomOp::Add,
            nir_atomic_op_imin => AtomOp::Min,
            nir_atomic_op_umin => AtomOp::Min,
            nir_atomic_op_imax => AtomOp::Max,
            nir_atomic_op_umax => AtomOp::Max,
            nir_atomic_op_iand => AtomOp::And,
            nir_atomic_op_ior => AtomOp::Or,
            nir_atomic_op_ixor => AtomOp::Xor,
            nir_atomic_op_xchg => AtomOp::Exch,
            nir_atomic_op_fadd => AtomOp::Add,
            nir_atomic_op_fmin => AtomOp::Min,
            nir_atomic_op_fmax => AtomOp::Max,
            nir_atomic_op_cmpxchg => AtomOp::CmpExch(cmp_src),
            _ => panic!("Unsupported NIR atomic op"),
        }
    }

    fn get_eviction_priority(
        &mut self,
        access: gl_access_qualifier,
    ) -> MemEvictionPriority {
        if self.sm.sm() >= 70 && access & ACCESS_NON_TEMPORAL != 0 {
            MemEvictionPriority::First
        } else {
            MemEvictionPriority::Normal
        }
    }

    fn get_image_dim(&mut self, intrin: &nir_intrinsic_instr) -> ImageDim {
        let is_array = intrin.image_array();
        let image_dim = intrin.image_dim();
        match intrin.image_dim() {
            GLSL_SAMPLER_DIM_1D => {
                if is_array {
                    ImageDim::_1DArray
                } else {
                    ImageDim::_1D
                }
            }
            GLSL_SAMPLER_DIM_2D => {
                if is_array {
                    ImageDim::_2DArray
                } else {
                    ImageDim::_2D
                }
            }
            GLSL_SAMPLER_DIM_3D => {
                assert!(!is_array);
                ImageDim::_3D
            }
            GLSL_SAMPLER_DIM_CUBE => ImageDim::_2DArray,
            GLSL_SAMPLER_DIM_BUF => {
                assert!(!is_array);
                ImageDim::_1DBuffer
            }
            _ => panic!("Unsupported image dimension: {}", image_dim),
        }
    }

    fn get_image_mem_type(&self, intrin: &nir_intrinsic_instr) -> MemType {
        match intrin.format() {
            PIPE_FORMAT_R8_UINT => MemType::U8,
            PIPE_FORMAT_R8_SINT => MemType::I8,
            PIPE_FORMAT_R16_UINT => MemType::U16,
            PIPE_FORMAT_R16_SINT => MemType::I16,
            PIPE_FORMAT_R32_UINT => MemType::B32,
            PIPE_FORMAT_R32G32_UINT => MemType::B64,
            PIPE_FORMAT_R32G32B32A32_UINT => MemType::B128,
            _ => panic!("Unknown format"),
        }
    }

    fn get_image_coord(
        &mut self,
        intrin: &nir_intrinsic_instr,
        dim: ImageDim,
    ) -> Src {
        let vec = self.get_ssa(intrin.get_src(1).as_def());
        // let sample = self.get_src(&srcs[2]);
        let comps = usize::from(dim.coord_comps());
        SSARef::try_from(&vec[0..comps]).unwrap().into()
    }

    fn parse_intrinsic(
        &mut self,
        b: &mut impl SSABuilder,
        intrin: &nir_intrinsic_instr,
    ) {
        let srcs = intrin.srcs_as_slice();
        match intrin.intrinsic {
            nir_intrinsic_al2p_nv => {
                let offset = self.get_src(&srcs[0]);
                let addr = u16::try_from(intrin.base()).unwrap();

                let flags = intrin.flags();
                let flags: nak_nir_attr_io_flags =
                    unsafe { std::mem::transmute_copy(&flags) };

                assert!(!flags.patch());

                let dst = b.alloc_ssa(RegFile::GPR);
                b.push_op(OpAL2P {
                    dst: dst.into(),
                    offset,
                    addr,
                    output: flags.output(),
                    comps: 1,
                });
                self.set_dst(&intrin.def, dst.into());
            }
            nir_intrinsic_ald_nv | nir_intrinsic_ast_nv => {
                let addr = u16::try_from(intrin.base()).unwrap();
                let base = u16::try_from(intrin.range_base()).unwrap();
                let range = u16::try_from(intrin.range()).unwrap();
                let range = base..(base + range);

                let flags = intrin.flags();
                let flags: nak_nir_attr_io_flags =
                    unsafe { std::mem::transmute_copy(&flags) };
                assert!(!flags.patch() || !flags.phys());

                if let ShaderIoInfo::Vtg(io) = &mut self.info.io {
                    if flags.patch() {
                        match &mut self.info.stage {
                            ShaderStageInfo::TessellationInit(stage) => {
                                assert!(flags.output());
                                stage.per_patch_attribute_count = max(
                                    stage.per_patch_attribute_count,
                                    (range.end / 4).try_into().unwrap(),
                                );
                            }
                            ShaderStageInfo::Tessellation(_) => (),
                            _ => panic!("Patch I/O not supported"),
                        }
                    } else {
                        if flags.output() {
                            if intrin.intrinsic == nir_intrinsic_ast_nv {
                                io.mark_store_req(range.clone());
                            }
                            io.mark_attrs_written(range);
                        } else {
                            io.mark_attrs_read(range);
                        }
                    }
                } else {
                    panic!("Must be a VTG stage");
                }

                let comps = intrin.num_components;

                if intrin.intrinsic == nir_intrinsic_ald_nv {
                    let vtx = self.get_src(&srcs[0]);
                    let offset = self.get_src(&srcs[1]);

                    assert!(intrin.def.bit_size() == 32);
                    let dst = b.alloc_ssa_vec(RegFile::GPR, comps);
                    b.push_op(OpALd {
                        dst: dst.clone().into(),
                        vtx,
                        addr,
                        offset,
                        comps,
                        patch: flags.patch(),
                        output: flags.output(),
                        phys: flags.phys(),
                    });
                    self.set_dst(&intrin.def, dst);
                } else if intrin.intrinsic == nir_intrinsic_ast_nv {
                    assert!(srcs[0].bit_size() == 32);
                    let data = self.get_src(&srcs[0]);
                    let vtx = self.get_src(&srcs[1]);
                    let offset = self.get_src(&srcs[2]);

                    b.push_op(OpASt {
                        data,
                        vtx,
                        addr,
                        offset,
                        comps,
                        patch: flags.patch(),
                        phys: flags.phys(),
                    });
                } else {
                    panic!("Invalid VTG I/O intrinsic");
                }
            }
            nir_intrinsic_as_uniform => {
                let src = self.get_ssa(srcs[0].as_def());
                let mut dst = Vec::new();
                for comp in src {
                    let u = b.alloc_ssa(comp.file().to_uniform().unwrap());
                    b.push_op(OpR2UR {
                        src: [*comp].into(),
                        dst: u.into(),
                    });
                    dst.push(u);
                }
                self.set_ssa(&intrin.def, dst);
            }
            nir_intrinsic_convert_alu_types => {
                let src_base_type = intrin.src_type().base_type();
                let src_bit_size = intrin.src_type().bit_size();
                let dst_base_type = intrin.dest_type().base_type();
                let dst_bit_size = intrin.dest_type().bit_size();
                let rnd_mode = f_rnd_mode_from_nir(intrin.rounding_mode());

                assert!(srcs[0].as_def().bit_size() == src_bit_size);
                assert!(intrin.def.bit_size() == dst_bit_size);

                let def_bits =
                    intrin.def.bit_size() * intrin.def.num_components();
                let dst = b.alloc_ssa_vec(RegFile::GPR, def_bits.div_ceil(32));

                match dst_base_type {
                    ALUType::INT | ALUType::UINT => {
                        let dst_type = IntType::from_bits(
                            dst_bit_size.into(),
                            dst_base_type == ALUType::INT,
                        );
                        match src_base_type {
                            ALUType::INT | ALUType::UINT => {
                                let src_type = IntType::from_bits(
                                    src_bit_size.into(),
                                    src_base_type == ALUType::INT,
                                );
                                b.push_op(OpI2I {
                                    dst: dst.clone().into(),
                                    src: self.get_src(&srcs[0]),
                                    src_type,
                                    dst_type,
                                    abs: false,
                                    neg: false,
                                    saturate: intrin.saturate(),
                                });
                            }
                            ALUType::FLOAT => {
                                let src_type =
                                    FloatType::from_bits(src_bit_size.into());
                                // F2I doesn't support 8-bit destinations
                                // pre-Volta
                                assert!(b.sm() >= 70 || dst_bit_size > 8);
                                b.push_op(OpF2I {
                                    dst: dst.clone().into(),
                                    src: self.get_src(&srcs[0]),
                                    src_type,
                                    dst_type,
                                    rnd_mode,
                                    ftz: self.float_ctl[src_type].ftz,
                                });
                            }
                            _ => panic!("Unknown src_type"),
                        }
                    }
                    ALUType::FLOAT => {
                        let dst_type =
                            FloatType::from_bits(dst_bit_size.into());
                        match src_base_type {
                            ALUType::INT | ALUType::UINT => {
                                let src_type = IntType::from_bits(
                                    src_bit_size.into(),
                                    src_base_type == ALUType::INT,
                                );
                                b.push_op(OpI2F {
                                    dst: dst.clone().into(),
                                    src: self.get_src(&srcs[0]),
                                    src_type,
                                    dst_type,
                                    rnd_mode,
                                });
                            }
                            ALUType::FLOAT => {
                                let src_type =
                                    FloatType::from_bits(src_bit_size.into());
                                b.push_op(OpF2F {
                                    dst: dst.clone().into(),
                                    src: self.get_src(&srcs[0]),
                                    src_type,
                                    dst_type,
                                    rnd_mode,
                                    ftz: self.float_ctl[src_type].ftz,
                                    high: false,
                                    integer_rnd: false,
                                });
                            }
                            _ => panic!("Unknown src_type"),
                        }
                    }
                    _ => panic!("Unknown dest_type"),
                }
                self.set_dst(&intrin.def, dst);
            }
            nir_intrinsic_ddx
            | nir_intrinsic_ddx_coarse
            | nir_intrinsic_ddx_fine => {
                // TODO: Real coarse derivatives

                assert!(intrin.def.bit_size() == 32);
                let ftype = FloatType::F32;

                let dst = b.alloc_ssa(RegFile::GPR);

                if self.sm.sm() >= 50 {
                    let scratch = b.alloc_ssa(RegFile::GPR);

                    b.push_op(OpShfl {
                        dst: scratch.into(),
                        in_bounds: Dst::None,
                        src: self.get_src(&srcs[0]),
                        lane: 1_u32.into(),
                        c: (0x3_u32 | (0x1c_u32 << 8)).into(),
                        op: ShflOp::Bfly,
                    });

                    // Starting with Blackwell, the shader stage now affects
                    // fswzadd so we need to use fswzadd.ndv
                    let deriv_mode = if self.sm.sm() >= 100 {
                        TexDerivMode::NonDivergent
                    } else {
                        TexDerivMode::Auto
                    };

                    b.push_op(OpFSwzAdd {
                        dst: dst.into(),
                        srcs: [scratch.into(), self.get_src(&srcs[0])],
                        ops: [
                            FSwzAddOp::SubLeft,
                            FSwzAddOp::SubRight,
                            FSwzAddOp::SubLeft,
                            FSwzAddOp::SubRight,
                        ],
                        rnd_mode: self.float_ctl[ftype].rnd_mode,
                        ftz: self.float_ctl[ftype].ftz,
                        deriv_mode,
                    });
                } else {
                    b.push_op(OpFSwz {
                        dst: dst.into(),
                        srcs: [self.get_src(&srcs[0]), self.get_src(&srcs[0])],
                        ops: [
                            FSwzAddOp::SubLeft,
                            FSwzAddOp::SubRight,
                            FSwzAddOp::SubLeft,
                            FSwzAddOp::SubRight,
                        ],
                        rnd_mode: self.float_ctl[ftype].rnd_mode,
                        ftz: self.float_ctl[ftype].ftz,
                        deriv_mode: TexDerivMode::Auto,
                        shuffle: FSwzShuffle::SwapHorizontal,
                    });
                }

                self.set_dst(&intrin.def, dst.into());
            }
            nir_intrinsic_ddy
            | nir_intrinsic_ddy_coarse
            | nir_intrinsic_ddy_fine => {
                // TODO: Real coarse derivatives

                assert!(intrin.def.bit_size() == 32);
                let ftype = FloatType::F32;
                let dst = b.alloc_ssa(RegFile::GPR);

                if self.sm.sm() >= 50 {
                    let scratch = b.alloc_ssa(RegFile::GPR);

                    b.push_op(OpShfl {
                        dst: scratch.into(),
                        in_bounds: Dst::None,
                        src: self.get_src(&srcs[0]),
                        lane: 2_u32.into(),
                        c: (0x3_u32 | (0x1c_u32 << 8)).into(),
                        op: ShflOp::Bfly,
                    });

                    // Starting with Blackwell, the shader stage now affects
                    // fswzadd so we need to use fswzadd.ndv
                    let deriv_mode = if self.sm.sm() >= 100 {
                        TexDerivMode::NonDivergent
                    } else {
                        TexDerivMode::Auto
                    };

                    b.push_op(OpFSwzAdd {
                        dst: dst.into(),
                        srcs: [scratch.into(), self.get_src(&srcs[0])],
                        ops: [
                            FSwzAddOp::SubLeft,
                            FSwzAddOp::SubLeft,
                            FSwzAddOp::SubRight,
                            FSwzAddOp::SubRight,
                        ],
                        rnd_mode: self.float_ctl[ftype].rnd_mode,
                        ftz: self.float_ctl[ftype].ftz,
                        deriv_mode,
                    });
                } else {
                    b.push_op(OpFSwz {
                        dst: dst.into(),
                        srcs: [self.get_src(&srcs[0]), self.get_src(&srcs[0])],
                        ops: [
                            FSwzAddOp::SubLeft,
                            FSwzAddOp::SubLeft,
                            FSwzAddOp::SubRight,
                            FSwzAddOp::SubRight,
                        ],
                        rnd_mode: self.float_ctl[ftype].rnd_mode,
                        ftz: self.float_ctl[ftype].ftz,
                        deriv_mode: TexDerivMode::Auto,
                        shuffle: FSwzShuffle::SwapVertical,
                    });
                }

                self.set_dst(&intrin.def, dst.into());
            }
            nir_intrinsic_ballot => {
                assert!(srcs[0].bit_size() == 1);
                let src = self.get_src(&srcs[0]);

                assert!(intrin.def.bit_size() == 32);
                let dst = b.alloc_ssa(RegFile::GPR);

                b.push_op(OpVote {
                    op: VoteOp::Any,
                    ballot: dst.into(),
                    vote: Dst::None,
                    pred: src,
                });
                self.set_dst(&intrin.def, dst.into());
            }
            nir_intrinsic_bar_break_nv => {
                let src = self.get_src(&srcs[0]);
                let bar_in = b.bmov_to_bar(src);
                let cond = self.get_src(&srcs[1]);

                let bar_out = b.alloc_ssa(RegFile::Bar);
                b.push_op(OpBreak {
                    bar_out: bar_out.into(),
                    bar_in: bar_in.into(),
                    cond,
                });

                self.set_dst(&intrin.def, b.bmov_to_gpr(bar_out.into()).into());
            }
            nir_intrinsic_bar_set_nv => {
                let label = self.label_alloc.alloc();
                let old = self.bar_label.insert(intrin.def.index, label);
                assert!(old.is_none());

                let bar_clear = b.alloc_ssa(RegFile::Bar);
                b.push_op(OpBClear {
                    dst: bar_clear.into(),
                });

                let bar_out = b.alloc_ssa(RegFile::Bar);
                b.push_op(OpBSSy {
                    bar_out: bar_out.into(),
                    bar_in: bar_clear.into(),
                    cond: SrcRef::True.into(),
                    target: label,
                });

                self.set_dst(&intrin.def, b.bmov_to_gpr(bar_out.into()).into());
            }
            nir_intrinsic_bar_sync_nv => {
                let src = self.get_src(&srcs[0]);

                let bar = b.bmov_to_bar(src);
                b.push_op(OpBSync {
                    bar: bar.into(),
                    cond: SrcRef::True.into(),
                });

                let bar_set_idx = &srcs[1].as_def().index;
                if let Some(label) = self.bar_label.get(bar_set_idx) {
                    b.push_op(OpNop {
                        label: Some(*label),
                    });
                }
            }
            nir_intrinsic_bindless_image_atomic
            | nir_intrinsic_bindless_image_atomic_swap => {
                let handle = self.get_src(&srcs[0]);
                let dim = self.get_image_dim(intrin);
                let coord = self.get_image_coord(intrin, dim);
                // let sample = self.get_src(&srcs[2]);
                let atom_type = self.get_atomic_type(intrin);
                let atom_op = self.get_atomic_op(intrin, AtomCmpSrc::Packed);

                assert!(
                    intrin.def.bit_size() == 32 || intrin.def.bit_size() == 64
                );
                assert!(intrin.def.num_components() == 1);
                let dst =
                    b.alloc_ssa_vec(RegFile::GPR, intrin.def.bit_size() / 32);

                let data = if intrin.intrinsic
                    == nir_intrinsic_bindless_image_atomic_swap
                {
                    if intrin.def.bit_size() == 64 {
                        SSARef::from([
                            self.get_ssa(srcs[3].as_def())[0],
                            self.get_ssa(srcs[3].as_def())[1],
                            self.get_ssa(srcs[4].as_def())[0],
                            self.get_ssa(srcs[4].as_def())[1],
                        ])
                        .into()
                    } else {
                        SSARef::from([
                            self.get_ssa(srcs[3].as_def())[0],
                            self.get_ssa(srcs[4].as_def())[0],
                        ])
                        .into()
                    }
                } else {
                    self.get_src(&srcs[3])
                };

                let is_reduction =
                    atom_op.is_reduction() && intrin.def.components_read() == 0;

                b.push_op(OpSuAtom {
                    dst: if self.sm.sm() >= 70 && is_reduction {
                        Dst::None
                    } else {
                        dst.clone().into()
                    },
                    fault: Dst::None,
                    handle: handle,
                    coord: coord,
                    data: data,
                    atom_op: atom_op,
                    atom_type: atom_type,
                    image_dim: dim,
                    mem_order: MemOrder::Strong(MemScope::GPU),
                    mem_eviction_priority: self
                        .get_eviction_priority(intrin.access()),
                });
                self.set_dst(&intrin.def, dst);
            }
            nir_intrinsic_suclamp_nv => {
                let coords = self.get_src(&srcs[0]);
                let params = self.get_src(&srcs[1]);

                let flags = intrin.flags();
                let flags: nak_nir_suclamp_flags =
                    unsafe { std::mem::transmute_copy(&flags) };

                let mode = match flags.mode() {
                    NAK_SUCLAMP_MODE_BLOCK_LINEAR => SuClampMode::BlockLinear,
                    NAK_SUCLAMP_MODE_PITCH_LINEAR => SuClampMode::PitchLinear,
                    NAK_SUCLAMP_MODE_STORED_DESCRIPTOR => {
                        SuClampMode::StoredInDescriptor
                    }
                    _ => panic!("Invalid suclamp mode"),
                };

                let round = match flags.round() {
                    NAK_SUCLAMP_ROUND_R1 => SuClampRound::R1,
                    NAK_SUCLAMP_ROUND_R2 => SuClampRound::R2,
                    NAK_SUCLAMP_ROUND_R4 => SuClampRound::R4,
                    NAK_SUCLAMP_ROUND_R8 => SuClampRound::R8,
                    NAK_SUCLAMP_ROUND_R16 => SuClampRound::R16,
                    _ => panic!("Invalid suclamp round"),
                };

                let dst = b.alloc_ssa(RegFile::GPR);
                let out_of_bounds = b.alloc_ssa(RegFile::Pred);
                b.push_op(OpSuClamp {
                    dst: dst.into(),
                    out_of_bounds: out_of_bounds.into(),
                    coords,
                    params,
                    mode,
                    round,
                    is_2d: flags.is_2d(),
                    is_s32: flags.is_s32(),
                    imm: 0,
                });
                let final_dst =
                    vec![dst, b.sel(out_of_bounds.into(), 1.into(), 0.into())];

                self.set_ssa(&intrin.def, final_dst);
            }
            nir_intrinsic_subfm_nv => {
                let x = self.get_src(&srcs[0]);
                let y = self.get_src(&srcs[1]);
                let z = self.get_src(&srcs[2]);
                let is_3d = intrin.flags() != 0;

                let dst = b.alloc_ssa(RegFile::GPR);
                let out_of_bounds = b.alloc_ssa(RegFile::Pred);
                b.push_op(OpSuBfm {
                    dst: dst.into(),
                    pdst: out_of_bounds.into(),
                    srcs: [x, y, z],
                    is_3d,
                });
                let final_dst =
                    vec![dst, b.sel(out_of_bounds.into(), 1.into(), 0.into())];

                self.set_ssa(&intrin.def, final_dst);
            }
            nir_intrinsic_sueau_nv => {
                let off = self.get_src(&srcs[0]);
                let bit_field = self.get_src(&srcs[1]);
                let addr = self.get_src(&srcs[2]);

                let dst = b.alloc_ssa(RegFile::GPR);
                b.push_op(OpSuEau {
                    dst: dst.into(),
                    off,
                    bit_field,
                    addr,
                });
                self.set_dst(&intrin.def, dst.into());
            }
            nir_intrinsic_imadsp_nv => {
                let src0 = self.get_src(&srcs[0]);
                let src1 = self.get_src(&srcs[1]);
                let src2 = self.get_src(&srcs[2]);

                let flags = intrin.flags();
                let flags: nak_nir_imadsp_flags =
                    unsafe { std::mem::transmute_copy(&flags) };

                let translate_src_type = |s| {
                    use IMadSpSrcType::*;
                    match s {
                        NAK_IMAD_TYPE_U32 => U32,
                        NAK_IMAD_TYPE_U24 => U24,
                        NAK_IMAD_TYPE_U16_LO => U16Lo,
                        NAK_IMAD_TYPE_U16_HI => U16Hi,
                        NAK_IMAD_TYPE_S32 => S32,
                        NAK_IMAD_TYPE_S24 => S24,
                        NAK_IMAD_TYPE_S16_LO => S16Lo,
                        NAK_IMAD_TYPE_S16_HI => S16Hi,
                        _ => panic!("Invalid imadsp mode"),
                    }
                };

                let mode = if flags.params_from_src1() {
                    IMadSpMode::FromSrc1
                } else {
                    IMadSpMode::Explicit([
                        translate_src_type(flags.src0()),
                        translate_src_type(flags.src1()),
                        translate_src_type(flags.src2()),
                    ])
                };

                let dst = b.alloc_ssa(RegFile::GPR);
                b.push_op(OpIMadSp {
                    srcs: [src0, src1, src2],
                    dst: dst.into(),
                    mode,
                });
                self.set_dst(&intrin.def, dst.into());
            }
            nir_intrinsic_suldga_nv => {
                let addr = self.get_src(&srcs[0]);
                let format = self.get_src(&srcs[1]);
                let out_of_bounds = self.get_src(&srcs[2]);

                let comps = intrin.num_components;

                assert!(intrin.def.bit_size() == 32);
                let mem_type = self.get_image_mem_type(intrin);

                let flags = intrin.flags();
                let offset_mode = match flags {
                    NAK_SUGA_OFF_MODE_U32 => SuGaOffsetMode::U32,
                    NAK_SUGA_OFF_MODE_S32 => SuGaOffsetMode::S32,
                    NAK_SUGA_OFF_MODE_U8 => SuGaOffsetMode::U8,
                    NAK_SUGA_OFF_MODE_S8 => SuGaOffsetMode::S8,
                    _ => panic!("Invalid suldga flags"),
                };

                let mem_order = if (intrin.access() & ACCESS_CAN_REORDER) != 0 {
                    MemOrder::Constant
                } else {
                    MemOrder::Strong(MemScope::GPU)
                };
                let cache_op = LdCacheOp::select(
                    self.sm,
                    MemSpace::Global(MemAddrType::A64),
                    mem_order,
                    self.get_eviction_priority(intrin.access()),
                );

                let dst = b.alloc_ssa_vec(RegFile::GPR, comps);
                b.push_op(OpSuLdGa {
                    dst: dst.clone().into(),
                    addr,
                    format,
                    out_of_bounds,
                    mem_type,
                    offset_mode,
                    cache_op,
                });
                self.set_dst(&intrin.def, dst);
            }
            nir_intrinsic_bindless_image_load => {
                let handle = self.get_src(&srcs[0]);
                let dim = self.get_image_dim(intrin);
                let coord = self.get_image_coord(intrin, dim);
                // let sample = self.get_src(&srcs[2]);

                let mem_order = if (intrin.access() & ACCESS_CAN_REORDER) != 0 {
                    MemOrder::Constant
                } else {
                    MemOrder::Strong(MemScope::GPU)
                };

                let comps = intrin.num_components;
                assert!(intrin.def.bit_size() == 32);
                assert!(comps == 1 || comps == 2 || comps == 4);
                let image_access =
                    ImageAccess::Formatted(ChannelMask::for_comps(comps));

                let dst = b.alloc_ssa_vec(RegFile::GPR, comps);

                b.push_op(OpSuLd {
                    dst: dst.clone().into(),
                    fault: Dst::None,
                    image_access,
                    image_dim: dim,
                    mem_order,
                    mem_eviction_priority: self
                        .get_eviction_priority(intrin.access()),
                    handle: handle,
                    coord: coord,
                });
                self.set_dst(&intrin.def, dst);
            }
            nir_intrinsic_bindless_image_sparse_load => {
                let handle = self.get_src(&srcs[0]);
                let dim = self.get_image_dim(intrin);
                let coord = self.get_image_coord(intrin, dim);
                // let sample = self.get_src(&srcs[2]);

                let mem_order = if (intrin.access() & ACCESS_CAN_REORDER) != 0 {
                    MemOrder::Constant
                } else {
                    MemOrder::Strong(MemScope::GPU)
                };

                let comps = intrin.num_components;
                assert!(intrin.def.bit_size() == 32);
                assert!(comps == 5);
                let image_access =
                    ImageAccess::Formatted(ChannelMask::for_comps(comps - 1));

                let dst = b.alloc_ssa_vec(RegFile::GPR, comps - 1);
                let fault = b.alloc_ssa(RegFile::Pred);

                b.push_op(OpSuLd {
                    dst: dst.clone().into(),
                    fault: fault.into(),
                    image_access,
                    image_dim: dim,
                    mem_order,
                    mem_eviction_priority: self
                        .get_eviction_priority(intrin.access()),
                    handle: handle,
                    coord: coord,
                });

                let mut final_dst = Vec::new();
                for i in 0..usize::from(comps) - 1 {
                    final_dst.push(dst[i]);
                }
                final_dst.push(b.sel(fault.into(), 0.into(), 1.into()));

                self.set_ssa(&intrin.def, final_dst);
            }
            nir_intrinsic_sustga_nv => {
                let addr = self.get_src(&srcs[0]);
                let format = self.get_src(&srcs[1]);
                let out_of_bounds = self.get_src(&srcs[2]);

                let data = self.get_src(&srcs[3]);
                let image_access =
                    ImageAccess::Formatted(ChannelMask::new(0xf));

                let flags = intrin.flags();
                let offset_mode = match flags {
                    NAK_SUGA_OFF_MODE_U32 => SuGaOffsetMode::U32,
                    NAK_SUGA_OFF_MODE_S32 => SuGaOffsetMode::S32,
                    NAK_SUGA_OFF_MODE_U8 => SuGaOffsetMode::U8,
                    NAK_SUGA_OFF_MODE_S8 => SuGaOffsetMode::S8,
                    _ => panic!("Invalid sustga flags"),
                };

                let cache_op = StCacheOp::select(
                    self.sm,
                    MemSpace::Global(MemAddrType::A64),
                    MemOrder::Strong(MemScope::GPU),
                    self.get_eviction_priority(intrin.access()),
                );

                b.push_op(OpSuStGa {
                    addr,
                    format,
                    data,
                    out_of_bounds,
                    image_access,
                    offset_mode,
                    cache_op,
                });
            }
            nir_intrinsic_bindless_image_store => {
                let handle = self.get_src(&srcs[0]);
                let dim = self.get_image_dim(intrin);
                let coord = self.get_image_coord(intrin, dim);
                // let sample = self.get_src(&srcs[2]);
                let data = self.get_src(&srcs[3]);

                let comps = intrin.num_components;
                assert!(srcs[3].bit_size() == 32);
                assert!(comps == 1 || comps == 2 || comps == 4);
                let image_access =
                    ImageAccess::Formatted(ChannelMask::for_comps(comps));

                b.push_op(OpSuSt {
                    image_access,
                    image_dim: dim,
                    mem_order: MemOrder::Strong(MemScope::GPU),
                    mem_eviction_priority: self
                        .get_eviction_priority(intrin.access()),
                    handle: handle,
                    coord: coord,
                    data: data,
                });
            }
            nir_intrinsic_copy_fs_outputs_nv => {
                let ShaderIoInfo::Fragment(info) = &mut self.info.io else {
                    panic!(
                        "copy_fs_outputs_nv is only allowed in fragment shaders"
                    );
                };

                for i in 0..32 {
                    if !self.fs_out_regs[i].is_none() {
                        info.writes_color |= 1 << i;
                    }
                }
                let mask_idx = (NAK_FS_OUT_SAMPLE_MASK / 4) as usize;
                info.writes_sample_mask = !self.fs_out_regs[mask_idx].is_none();
                let depth_idx = (NAK_FS_OUT_DEPTH / 4) as usize;
                info.writes_depth = !self.fs_out_regs[depth_idx].is_none();

                let mut srcs = Vec::new();
                for i in 0..8 {
                    // Even though the mask is per-component, the actual output
                    // space is per-output vec4s.
                    if info.writes_color & (0xf << (i * 4)) != 0 {
                        for c in 0..4 {
                            match self.fs_out_regs[i * 4 + c] {
                                None => srcs.push(b.undef().into()),
                                Some(reg) => srcs.push(reg.into()),
                            }
                        }
                    }
                }

                // These always come together for some reason
                if info.writes_sample_mask || info.writes_depth {
                    if info.writes_sample_mask {
                        srcs.push(self.fs_out_regs[mask_idx].unwrap().into());
                    } else {
                        srcs.push(b.undef().into());
                    }
                    if info.writes_depth {
                        srcs.push(self.fs_out_regs[depth_idx].unwrap().into());
                    }
                }

                b.push_op(OpRegOut { srcs: srcs });
            }
            nir_intrinsic_demote => {
                if let ShaderStageInfo::Fragment(info) = &mut self.info.stage {
                    info.uses_kill = true;
                } else {
                    panic!("OpKill is only available in fragment shaders");
                }
                b.push_op(OpKill {});
            }
            nir_intrinsic_demote_if => {
                if let ShaderStageInfo::Fragment(info) = &mut self.info.stage {
                    info.uses_kill = true;
                } else {
                    panic!("OpKill is only available in fragment shaders");
                }
                let cond = self.get_ssa(srcs[0].as_def())[0];
                b.predicate(cond.into()).push_op(OpKill {});
            }
            nir_intrinsic_global_atomic => {
                let bit_size = intrin.def.bit_size();
                let (addr, offset) = self.get_io_addr_offset(&srcs[0], 24);
                let data = self.get_src(&srcs[1]);
                let atom_type = self.get_atomic_type(intrin);
                let atom_op = self.get_atomic_op(intrin, AtomCmpSrc::Separate);

                assert!(intrin.def.num_components() == 1);
                let dst = b.alloc_ssa_vec(RegFile::GPR, bit_size.div_ceil(32));

                let is_reduction =
                    atom_op.is_reduction() && intrin.def.components_read() == 0;

                b.push_op(OpAtom {
                    dst: if is_reduction {
                        Dst::None
                    } else {
                        dst.clone().into()
                    },
                    addr: addr,
                    cmpr: 0.into(),
                    data: data,
                    atom_op: atom_op,
                    atom_type: atom_type,
                    addr_offset: offset,
                    mem_space: MemSpace::Global(MemAddrType::A64),
                    mem_order: MemOrder::Strong(MemScope::GPU),
                    mem_eviction_priority: MemEvictionPriority::Normal, // Note: no intrinic access
                });
                self.set_dst(&intrin.def, dst);
            }
            nir_intrinsic_global_atomic_swap => {
                assert!(intrin.atomic_op() == nir_atomic_op_cmpxchg);
                let bit_size = intrin.def.bit_size();
                let (addr, offset) = self.get_io_addr_offset(&srcs[0], 24);
                let cmpr = self.get_src(&srcs[1]);
                let data = self.get_src(&srcs[2]);
                let atom_type = AtomType::U(bit_size);

                assert!(intrin.def.num_components() == 1);
                let dst = b.alloc_ssa_vec(RegFile::GPR, bit_size.div_ceil(32));

                b.push_op(OpAtom {
                    dst: dst.clone().into(),
                    addr: addr,
                    cmpr: cmpr,
                    data: data,
                    atom_op: AtomOp::CmpExch(AtomCmpSrc::Separate),
                    atom_type: atom_type,
                    addr_offset: offset,
                    mem_space: MemSpace::Global(MemAddrType::A64),
                    mem_order: MemOrder::Strong(MemScope::GPU),
                    mem_eviction_priority: MemEvictionPriority::Normal, // Note: no intrinic access
                });
                self.set_dst(&intrin.def, dst);
            }
            nir_intrinsic_ipa_nv => {
                let addr = u16::try_from(intrin.base()).unwrap();

                let flags = intrin.flags();
                let flags: nak_nir_ipa_flags =
                    unsafe { std::mem::transmute_copy(&flags) };

                let mode = match flags.interp_mode() {
                    NAK_INTERP_MODE_PERSPECTIVE => PixelImap::Perspective,
                    NAK_INTERP_MODE_SCREEN_LINEAR => PixelImap::ScreenLinear,
                    NAK_INTERP_MODE_CONSTANT => PixelImap::Constant,
                    _ => panic!("Unsupported interp mode"),
                };

                let freq = match flags.interp_freq() {
                    NAK_INTERP_FREQ_PASS => InterpFreq::Pass,
                    NAK_INTERP_FREQ_PASS_MUL_W => InterpFreq::PassMulW,
                    NAK_INTERP_FREQ_CONSTANT => InterpFreq::Constant,
                    NAK_INTERP_FREQ_STATE => InterpFreq::State,
                    _ => panic!("Invalid interp freq"),
                };

                let loc = match flags.interp_loc() {
                    NAK_INTERP_LOC_DEFAULT => InterpLoc::Default,
                    NAK_INTERP_LOC_CENTROID => InterpLoc::Centroid,
                    NAK_INTERP_LOC_OFFSET => InterpLoc::Offset,
                    _ => panic!("Invalid interp loc"),
                };

                let inv_w = if freq == InterpFreq::PassMulW {
                    self.get_src(&srcs[0])
                } else {
                    0.into()
                };

                let offset = if loc == InterpLoc::Offset {
                    self.get_src(&srcs[1])
                } else {
                    0.into()
                };

                let ShaderIoInfo::Fragment(io) = &mut self.info.io else {
                    panic!("OpIpa is only used for fragment shaders");
                };

                io.mark_attr_read(addr, mode);

                let dst = b.alloc_ssa(RegFile::GPR);
                b.push_op(OpIpa {
                    dst: dst.into(),
                    addr: addr,
                    freq: freq,
                    loc: loc,
                    inv_w: inv_w,
                    offset: offset,
                });
                self.set_dst(&intrin.def, dst.into());
            }
            nir_intrinsic_isberd_nv => {
                let dst = b.alloc_ssa(RegFile::GPR);
                b.push_op(OpIsberd {
                    dst: dst.into(),
                    idx: self.get_src(&srcs[0]),
                });
                self.set_dst(&intrin.def, dst.into());
            }
            nir_intrinsic_vild_nv => {
                let dst = b.alloc_ssa(RegFile::GPR);

                let (idx, off) = self.get_io_addr_offset(&srcs[0], 8);
                b.push_op(OpViLd {
                    dst: dst.into(),
                    idx,
                    off: off.try_into().unwrap(),
                });
                self.set_dst(&intrin.def, dst.into());
            }
            nir_intrinsic_load_barycentric_at_offset_nv => (),
            nir_intrinsic_load_barycentric_centroid => (),
            nir_intrinsic_load_barycentric_pixel => (),
            nir_intrinsic_load_barycentric_sample => (),
            nir_intrinsic_load_global | nir_intrinsic_load_global_constant => {
                let size_B =
                    (intrin.def.bit_size() / 8) * intrin.def.num_components();
                assert!(u32::from(size_B) <= intrin.align());
                let order = if intrin.intrinsic
                    == nir_intrinsic_load_global_constant
                    || (intrin.access() & ACCESS_CAN_REORDER) != 0
                {
                    MemOrder::Constant
                } else {
                    MemOrder::Strong(MemScope::GPU)
                };
                let access = MemAccess {
                    mem_type: MemType::from_size(size_B, false),
                    space: MemSpace::Global(MemAddrType::A64),
                    order: order,
                    eviction_priority: self
                        .get_eviction_priority(intrin.access()),
                };
                let (addr, offset) = self.get_io_addr_offset(&srcs[0], 24);
                let dst = b.alloc_ssa_vec(RegFile::GPR, size_B.div_ceil(4));

                b.push_op(OpLd {
                    dst: dst.clone().into(),
                    addr: addr,
                    offset: offset,
                    access: access,
                });
                self.set_dst(&intrin.def, dst);
            }
            nir_intrinsic_ldtram_nv => {
                let ShaderIoInfo::Fragment(io) = &mut self.info.io else {
                    panic!("ldtram_nv is only used for fragment shaders");
                };

                assert!(
                    intrin.def.bit_size() == 32
                        && intrin.def.num_components == 2
                );

                let flags = intrin.flags();
                let use_c = flags != 0;

                let addr = u16::try_from(intrin.base()).unwrap();

                io.mark_barycentric_attr_in(addr);

                let dst = b.alloc_ssa_vec(RegFile::GPR, 2);
                b.push_op(OpLdTram {
                    dst: dst.clone().into(),
                    addr,
                    use_c,
                });
                self.set_dst(&intrin.def, dst);
            }
            nir_intrinsic_load_sample_id => {
                let dst = b.alloc_ssa(RegFile::GPR);
                b.push_op(OpPixLd {
                    dst: dst.into(),
                    val: PixVal::MyIndex,
                });
                self.set_dst(&intrin.def, dst.into());
            }
            nir_intrinsic_load_sample_mask_in => {
                if let ShaderIoInfo::Fragment(info) = &mut self.info.io {
                    info.reads_sample_mask = true;
                } else {
                    panic!(
                        "sample_mask_in is only available in fragment shaders"
                    );
                }

                let dst = b.alloc_ssa(RegFile::GPR);
                b.push_op(OpPixLd {
                    dst: dst.into(),
                    val: PixVal::CovMask,
                });
                self.set_dst(&intrin.def, dst.into());
            }
            nir_intrinsic_load_tess_coord_xy => {
                // Loading gl_TessCoord in tessellation evaluation shaders is
                // weird.  It's treated as a per-vertex output which is indexed
                // by LANEID.
                match &self.info.stage {
                    ShaderStageInfo::Tessellation(_) => (),
                    _ => panic!(
                        "load_tess_coord is only available in tessellation \
                         shaders"
                    ),
                };

                assert!(intrin.def.bit_size() == 32);
                assert!(intrin.def.num_components() == 2);

                let vtx = b.alloc_ssa(RegFile::GPR);
                b.push_op(OpS2R {
                    dst: vtx.into(),
                    idx: 0,
                });

                // This is recorded as a patch output in parse_shader() because
                // the hardware requires it be in the SPH, whether we use it or
                // not.

                let comps = 2;
                let dst = b.alloc_ssa_vec(RegFile::GPR, comps);
                b.push_op(OpALd {
                    dst: dst.clone().into(),
                    vtx: vtx.into(),
                    addr: NAK_ATTR_TESS_COORD,
                    offset: 0.into(),
                    comps,
                    patch: false,
                    output: true,
                    phys: false,
                });
                self.set_dst(&intrin.def, dst);
            }
            nir_intrinsic_load_scratch => {
                let size_B =
                    (intrin.def.bit_size() / 8) * intrin.def.num_components();
                assert!(u32::from(size_B) <= intrin.align());
                let access = MemAccess {
                    mem_type: MemType::from_size(size_B, false),
                    space: MemSpace::Local,
                    order: MemOrder::Strong(MemScope::CTA),
                    eviction_priority: MemEvictionPriority::Normal,
                };
                let (addr, offset) = self.get_io_addr_offset(&srcs[0], 24);
                let dst = b.alloc_ssa_vec(RegFile::GPR, size_B.div_ceil(4));

                b.push_op(OpLd {
                    dst: dst.clone().into(),
                    addr: addr,
                    offset: offset,
                    access: access,
                });
                self.set_dst(&intrin.def, dst);
            }
            nir_intrinsic_load_shared => {
                let size_B =
                    (intrin.def.bit_size() / 8) * intrin.def.num_components();
                assert!(u32::from(size_B) <= intrin.align());
                let access = MemAccess {
                    mem_type: MemType::from_size(size_B, false),
                    space: MemSpace::Shared,
                    order: MemOrder::Strong(MemScope::CTA),
                    eviction_priority: MemEvictionPriority::Normal,
                };
                let (addr, offset) = self.get_io_addr_offset(&srcs[0], 24);
                let offset = offset + intrin.base();
                let dst = b.alloc_ssa_vec(RegFile::GPR, size_B.div_ceil(4));

                b.push_op(OpLd {
                    dst: dst.clone().into(),
                    addr: addr,
                    offset: offset,
                    access: access,
                });
                self.set_dst(&intrin.def, dst);
            }
            nir_intrinsic_load_shared_lock_nv => {
                let size_B = intrin.def.bit_size() / 8;
                let mem_type = MemType::from_size(size_B, false);

                let (addr, offset) = self.get_io_addr_offset(&srcs[0], 24);
                let dst = b.alloc_ssa_vec(RegFile::GPR, size_B.div_ceil(4));
                let locked = b.alloc_ssa(RegFile::Pred);

                b.push_op(OpLdSharedLock {
                    dst: dst.clone().into(),
                    locked: locked.clone().into(),
                    addr,
                    offset,
                    mem_type,
                });
                let locked_gpr = b.sel(locked.into(), 1.into(), 0.into());

                // for 32-bit we have 2x32 return type,
                // for 64-bit we need 2x64, so is_locked must be a 64-bit val.
                // we can fill the remaining SSAValue with a copy of is_locked
                let locked_dst = std::iter::repeat(locked_gpr).take(dst.len());
                let nir_dst: Vec<_> =
                    dst.iter().copied().chain(locked_dst).collect();
                self.set_ssa(intrin.def.as_def(), nir_dst);
            }
            nir_intrinsic_load_sysval_nv => {
                let idx = u8::try_from(intrin.base()).unwrap();
                debug_assert!(intrin.def.num_components == 1);
                debug_assert!(
                    intrin.def.bit_size == 32 || intrin.def.bit_size == 64
                );
                let comps = intrin.def.bit_size / 32;
                let dst = b.alloc_ssa_vec(RegFile::GPR, comps);
                if self.sm.sm() >= 50
                    && (idx == NAK_SV_CLOCK || idx == NAK_SV_CLOCK + 1)
                {
                    debug_assert!(idx + comps <= NAK_SV_CLOCK + 2);
                    b.push_op(OpCS2R {
                        dst: dst.clone().into(),
                        idx: idx,
                    });
                } else {
                    debug_assert!(intrin.def.bit_size == 32);
                    b.push_op(OpS2R {
                        dst: dst.clone().into(),
                        idx: idx,
                    });
                }
                self.set_dst(&intrin.def, dst);
            }
            nir_intrinsic_ldc_nv => {
                let size_B =
                    (intrin.def.bit_size() / 8) * intrin.def.num_components();
                let idx = &srcs[0];

                let (off, off_imm) = self.get_cbuf_addr_offset(&srcs[1]);

                let dst = b.alloc_ssa_vec(RegFile::GPR, size_B.div_ceil(4));

                if let Some(idx_imm) = idx.as_uint() {
                    let idx_imm: u8 = idx_imm.try_into().unwrap();
                    let cb = CBufRef {
                        buf: CBuf::Binding(idx_imm),
                        offset: off_imm,
                    };
                    if off.is_zero() {
                        for (i, comp) in dst.iter().enumerate() {
                            let i = u16::try_from(i).unwrap();
                            b.copy_to(
                                (*comp).into(),
                                cb.clone().offset(i * 4).into(),
                            );
                        }
                    } else {
                        b.push_op(OpLdc {
                            dst: dst.clone().into(),
                            cb: cb.into(),
                            offset: off,
                            mode: LdcMode::Indexed,
                            mem_type: MemType::from_size(size_B, false),
                        });
                    }
                } else {
                    // In the IndexedSegmented mode, the hardware computes the
                    // actual index and offset as follows:
                    //
                    //    idx = imm_idx + reg[31:16]
                    //    offset = imm_offset + reg[15:0]
                    //    ldc c[idx][offset]
                    //
                    // So pack the index and offset accordingly
                    let idx = self.get_src(idx);
                    let off_idx = b.prmt(off, idx, [0, 1, 4, 5]);
                    let cb = CBufRef {
                        buf: CBuf::Binding(0),
                        offset: off_imm,
                    };
                    b.push_op(OpLdc {
                        dst: dst.clone().into(),
                        cb: cb.into(),
                        offset: off_idx.into(),
                        mode: LdcMode::IndexedSegmented,
                        mem_type: MemType::from_size(size_B, false),
                    });
                }
                self.set_dst(&intrin.def, dst);
            }
            nir_intrinsic_ldcx_nv => {
                let size_B =
                    (intrin.def.bit_size() / 8) * intrin.def.num_components();

                let handle = self.get_ssa_ref(&srcs[0]);
                let (off, off_imm) = self.get_cbuf_addr_offset(&srcs[1]);

                let cb = CBufRef {
                    buf: CBuf::BindlessSSA(handle),
                    offset: off_imm,
                };

                let dst = b.alloc_ssa_vec(RegFile::GPR, size_B.div_ceil(4));
                if off.is_zero() {
                    for (i, comp) in dst.iter().enumerate() {
                        let i = u16::try_from(i).unwrap();
                        b.copy_to(
                            (*comp).into(),
                            cb.clone().offset(i * 4).into(),
                        );
                    }
                } else {
                    b.push_op(OpLdc {
                        dst: dst.clone().into(),
                        cb: cb.into(),
                        offset: off,
                        mode: LdcMode::Indexed,
                        mem_type: MemType::from_size(size_B, false),
                    });
                }
                self.set_dst(&intrin.def, dst);
            }
            nir_intrinsic_pin_cx_handle_nv => {
                let handle = self.get_ssa_ref(&srcs[0]);
                b.push_op(OpPin {
                    src: handle.clone().into(),
                    dst: handle.into(),
                });
            }
            nir_intrinsic_unpin_cx_handle_nv => {
                let handle = self.get_ssa_ref(&srcs[0]);
                b.push_op(OpUnpin {
                    src: handle.clone().into(),
                    dst: handle.into(),
                });
            }
            nir_intrinsic_barrier => {
                let modes = intrin.memory_modes();
                let semantics = intrin.memory_semantics();
                if (modes & nir_var_mem_global) != 0
                    && (semantics & NIR_MEMORY_RELEASE) != 0
                {
                    // Pre-Volta doesn't have WBAll but it also seems that we
                    // don't need it.
                    if self.sm.sm() >= 70 {
                        b.push_op(OpCCtl {
                            op: CCtlOp::WBAll,
                            mem_space: MemSpace::Global(MemAddrType::A64),
                            addr: 0.into(),
                            addr_offset: 0,
                        });
                    }
                }
                match intrin.execution_scope() {
                    SCOPE_NONE => (),
                    SCOPE_WORKGROUP => {
                        assert!(
                            self.nir.info.stage() == MESA_SHADER_COMPUTE
                                || self.nir.info.stage() == MESA_SHADER_KERNEL
                        );
                        self.info.num_control_barriers = 1;
                        b.push_op(OpBar {});
                    }
                    _ => panic!("Unhandled execution scope"),
                }
                if intrin.memory_scope() != SCOPE_NONE {
                    let mem_scope = match intrin.memory_scope() {
                        SCOPE_INVOCATION | SCOPE_SUBGROUP => MemScope::CTA,
                        // A membar.gpu is very expensive so use .cta whenever
                        // possible.
                        // TODO: Figure out under which conditions we can relax
                        //       them for global memory/images to CTA.
                        SCOPE_WORKGROUP => {
                            let global_modes = nir_var_image
                                | nir_var_mem_global
                                | nir_var_mem_ssbo;
                            if intrin.memory_modes() & global_modes != 0 {
                                MemScope::GPU
                            } else {
                                MemScope::CTA
                            }
                        }
                        SCOPE_QUEUE_FAMILY | SCOPE_DEVICE => MemScope::GPU,
                        _ => panic!("Unhandled memory scope"),
                    };
                    b.push_op(OpMemBar { scope: mem_scope });
                }
                if (modes & nir_var_mem_global) != 0
                    && (semantics & NIR_MEMORY_ACQUIRE) != 0
                {
                    b.push_op(OpCCtl {
                        op: CCtlOp::IVAll,
                        mem_space: MemSpace::Global(MemAddrType::A64),
                        addr: 0.into(),
                        addr_offset: 0,
                    });
                }
            }
            nir_intrinsic_quad_broadcast
            | nir_intrinsic_read_invocation
            | nir_intrinsic_shuffle
            | nir_intrinsic_shuffle_down
            | nir_intrinsic_shuffle_up
            | nir_intrinsic_shuffle_xor => {
                assert!(srcs[0].bit_size() == 32);
                assert!(srcs[0].num_components() == 1);
                let data = self.get_src(&srcs[0]);

                assert!(srcs[1].bit_size() == 32);
                let idx = self.get_src(&srcs[1]);

                assert!(intrin.def.bit_size() == 32);
                let dst = b.alloc_ssa(RegFile::GPR);

                b.push_op(OpShfl {
                    dst: dst.into(),
                    in_bounds: Dst::None,
                    src: data,
                    lane: idx,
                    c: match intrin.intrinsic {
                        nir_intrinsic_quad_broadcast => 0x1c_03.into(),
                        nir_intrinsic_shuffle_up => 0.into(),
                        _ => 0x1f.into(),
                    },
                    op: match intrin.intrinsic {
                        nir_intrinsic_shuffle_down => ShflOp::Down,
                        nir_intrinsic_shuffle_up => ShflOp::Up,
                        nir_intrinsic_shuffle_xor => ShflOp::Bfly,
                        _ => ShflOp::Idx,
                    },
                });
                self.set_dst(&intrin.def, dst.into());
            }
            nir_intrinsic_quad_swap_horizontal
            | nir_intrinsic_quad_swap_vertical
            | nir_intrinsic_quad_swap_diagonal => {
                assert!(srcs[0].bit_size() == 32);
                assert!(srcs[0].num_components() == 1);
                let data = self.get_src(&srcs[0]);

                assert!(intrin.def.bit_size() == 32);
                let dst = b.alloc_ssa(RegFile::GPR);
                b.push_op(OpShfl {
                    dst: dst.into(),
                    in_bounds: Dst::None,
                    src: data,
                    lane: match intrin.intrinsic {
                        nir_intrinsic_quad_swap_horizontal => 1_u32.into(),
                        nir_intrinsic_quad_swap_vertical => 2_u32.into(),
                        nir_intrinsic_quad_swap_diagonal => 3_u32.into(),
                        op => panic!("Unknown quad intrinsic {}", op),
                    },
                    c: 0x1c_03.into(),
                    op: ShflOp::Bfly,
                });
                self.set_dst(&intrin.def, dst.into());
            }
            nir_intrinsic_reduce => {
                assert!(srcs[0].bit_size() == 32);
                assert!(srcs[0].num_components() == 1);
                let src = self.get_src(&srcs[0]);
                let dst = b.alloc_ssa(RegFile::UGPR);

                let op = match intrin.reduction_op() {
                    nir_op_iand => ReduxOp::And,
                    nir_op_ior => ReduxOp::Or,
                    nir_op_ixor => ReduxOp::Xor,
                    nir_op_iadd => ReduxOp::Sum,
                    nir_op_imin => ReduxOp::Min(IntCmpType::I32),
                    nir_op_imax => ReduxOp::Max(IntCmpType::I32),
                    nir_op_umin => ReduxOp::Min(IntCmpType::U32),
                    nir_op_umax => ReduxOp::Max(IntCmpType::U32),
                    _ => panic!("Unknown reduction op"),
                };

                b.push_op(OpRedux {
                    dst: dst.into(),
                    src: src,
                    op,
                });
                self.set_dst(&intrin.def, dst.into());
            }
            nir_intrinsic_shared_atomic => {
                let bit_size = intrin.def.bit_size();
                let (addr, offset) = self.get_io_addr_offset(&srcs[0], 24);
                let data = self.get_src(&srcs[1]);
                let atom_type = self.get_atomic_type(intrin);
                let atom_op = self.get_atomic_op(intrin, AtomCmpSrc::Separate);

                assert!(intrin.def.num_components() == 1);
                let dst = b.alloc_ssa_vec(RegFile::GPR, bit_size.div_ceil(32));

                b.push_op(OpAtom {
                    dst: dst.clone().into(),
                    addr: addr,
                    cmpr: 0.into(),
                    data: data,
                    atom_op: atom_op,
                    atom_type: atom_type,
                    addr_offset: offset,
                    mem_space: MemSpace::Shared,
                    mem_order: MemOrder::Strong(MemScope::CTA),
                    mem_eviction_priority: MemEvictionPriority::Normal,
                });
                self.set_dst(&intrin.def, dst);
            }
            nir_intrinsic_shared_atomic_swap => {
                assert!(intrin.atomic_op() == nir_atomic_op_cmpxchg);
                let bit_size = intrin.def.bit_size();
                let (addr, offset) = self.get_io_addr_offset(&srcs[0], 24);
                let cmpr = self.get_src(&srcs[1]);
                let data = self.get_src(&srcs[2]);
                let atom_type = AtomType::U(bit_size);

                assert!(intrin.def.num_components() == 1);
                let dst = b.alloc_ssa_vec(RegFile::GPR, bit_size.div_ceil(32));

                b.push_op(OpAtom {
                    dst: dst.clone().into(),
                    addr: addr,
                    cmpr: cmpr,
                    data: data,
                    atom_op: AtomOp::CmpExch(AtomCmpSrc::Separate),
                    atom_type: atom_type,
                    addr_offset: offset,
                    mem_space: MemSpace::Shared,
                    mem_order: MemOrder::Strong(MemScope::CTA),
                    mem_eviction_priority: MemEvictionPriority::Normal,
                });
                self.set_dst(&intrin.def, dst);
            }
            nir_intrinsic_ssa_bar_nv => {
                let src = self.get_src(&srcs[0]);
                b.push_op(OpSrcBar { src });
            }
            nir_intrinsic_store_global => {
                let data = self.get_src(&srcs[0]);
                let size_B =
                    (srcs[0].bit_size() / 8) * srcs[0].num_components();
                assert!(u32::from(size_B) <= intrin.align());
                let access = MemAccess {
                    mem_type: MemType::from_size(size_B, false),
                    space: MemSpace::Global(MemAddrType::A64),
                    order: MemOrder::Strong(MemScope::GPU),
                    eviction_priority: self
                        .get_eviction_priority(intrin.access()),
                };
                let (addr, offset) = self.get_io_addr_offset(&srcs[1], 24);

                b.push_op(OpSt {
                    addr: addr,
                    data: data,
                    offset: offset,
                    access: access,
                });
            }
            nir_intrinsic_fs_out_nv => {
                let data = self.get_ssa(srcs[0].as_def());
                assert!(data.len() == 1);
                let data = data[0];

                let addr = u16::try_from(intrin.base()).unwrap();
                assert!(addr % 4 == 0);

                self.fs_out_regs[usize::from(addr / 4)] = Some(data);
            }
            nir_intrinsic_store_scratch => {
                let data = self.get_src(&srcs[0]);
                let size_B =
                    (srcs[0].bit_size() / 8) * srcs[0].num_components();
                assert!(u32::from(size_B) <= intrin.align());
                let access = MemAccess {
                    mem_type: MemType::from_size(size_B, false),
                    space: MemSpace::Local,
                    order: MemOrder::Strong(MemScope::CTA),
                    eviction_priority: MemEvictionPriority::Normal,
                };
                let (addr, offset) = self.get_io_addr_offset(&srcs[1], 24);

                b.push_op(OpSt {
                    addr: addr,
                    data: data,
                    offset: offset,
                    access: access,
                });
            }
            nir_intrinsic_store_shared => {
                let data = self.get_src(&srcs[0]);
                let size_B =
                    (srcs[0].bit_size() / 8) * srcs[0].num_components();
                assert!(u32::from(size_B) <= intrin.align());
                let access = MemAccess {
                    mem_type: MemType::from_size(size_B, false),
                    space: MemSpace::Shared,
                    order: MemOrder::Strong(MemScope::CTA),
                    eviction_priority: MemEvictionPriority::Normal,
                };
                let (addr, offset) = self.get_io_addr_offset(&srcs[1], 24);
                let offset = offset + intrin.base();

                b.push_op(OpSt {
                    addr: addr,
                    data: data,
                    offset: offset,
                    access: access,
                });
            }
            nir_intrinsic_store_shared_unlock_nv => {
                let data = self.get_src(&srcs[0]);
                let size_B =
                    (srcs[0].bit_size() / 8) * srcs[0].num_components();
                let mem_type = MemType::from_size(size_B, false);

                let (addr, offset) = self.get_io_addr_offset(&srcs[1], 24);
                let locked = b.alloc_ssa(RegFile::Pred);

                b.push_op(OpStSCheckUnlock {
                    locked: locked.clone().into(),
                    addr,
                    data,
                    offset,
                    mem_type,
                });
                let locked_gpr = b.sel(locked.into(), 1.into(), 0.into());
                self.set_dst(intrin.def.as_def(), locked_gpr.into());
            }
            nir_intrinsic_emit_vertex_nv | nir_intrinsic_end_primitive_nv => {
                assert!(intrin.def.bit_size() == 32);
                assert!(intrin.def.num_components() == 1);

                let dst = b.alloc_ssa(RegFile::GPR);
                let handle = self.get_src(&srcs[0]);
                let stream_id = intrin.stream_id();

                b.push_op(OpOut {
                    dst: dst.into(),
                    handle: handle,
                    stream: stream_id.into(),
                    out_type: if intrin.intrinsic
                        == nir_intrinsic_emit_vertex_nv
                    {
                        OutType::Emit
                    } else {
                        OutType::Cut
                    },
                });
                self.set_dst(&intrin.def, dst.into());
            }

            nir_intrinsic_final_primitive_nv => {
                let handle = self.get_src(&srcs[0]);

                if self.sm.sm() >= 70 {
                    b.push_op(OpOutFinal { handle: handle });
                } else {
                    b.push_op(OpRegOut { srcs: vec![handle] });
                }
            }
            nir_intrinsic_vote_all
            | nir_intrinsic_vote_any
            | nir_intrinsic_vote_ieq => {
                let src = self.get_src(&srcs[0]);
                let src_bits = srcs[0].bit_size() * srcs[0].num_components();

                assert!(intrin.def.bit_size() == 1);
                let dst = b.alloc_ssa(RegFile::Pred);

                if src_bits == 1 {
                    b.push_op(OpVote {
                        op: match intrin.intrinsic {
                            nir_intrinsic_vote_all => VoteOp::All,
                            nir_intrinsic_vote_any => VoteOp::Any,
                            nir_intrinsic_vote_ieq => VoteOp::Eq,
                            _ => panic!("Unknown vote intrinsic"),
                        },
                        ballot: Dst::None,
                        vote: dst.into(),
                        pred: src,
                    });
                } else {
                    assert_eq!(intrin.intrinsic, nir_intrinsic_vote_ieq);
                    b.push_op(OpMatch {
                        op: MatchOp::All,
                        mask: Dst::None,
                        pred: dst.into(),
                        src,
                        u64: match src_bits {
                            32 => false,
                            64 => true,
                            _ => panic!("Unsupported vote_ieq bit size"),
                        },
                    });
                }
                self.set_dst(&intrin.def, dst.into());
            }
            nir_intrinsic_is_sparse_texels_resident => {
                let src = self.get_src(&srcs[0]);
                let dst = b.isetp(IntCmpType::I32, IntCmpOp::Ne, src, 0.into());
                self.set_dst(&intrin.def, dst.into());
            }
            nir_intrinsic_cmat_muladd_nv => {
                let flags: nak_nir_cmat_mul_add_flags =
                    unsafe { std::mem::transmute(intrin.flags()) };
                let cmat_a = self.get_src(&srcs[0]);
                let cmat_b = self.get_src(&srcs[1]);
                let cmat_c = self.get_src(&srcs[2]);
                let dst_bit_size = intrin.def.bit_size();
                let dst = b.alloc_ssa_vec(
                    RegFile::GPR,
                    (intrin.def.num_components() * intrin.def.bit_size)
                        .div_ceil(32),
                );
                let dst_type = FloatType::from_bits(dst_bit_size.into());
                match flags.cmat_type() {
                    NAK_CMAT_TYPE_M16N8K8_FLOAT
                    | NAK_CMAT_TYPE_M16N8K16_FLOAT => {
                        let mat_size = match flags.cmat_type() {
                            NAK_CMAT_TYPE_M16N8K8_FLOAT => HmmaSize::M16N8K8,
                            NAK_CMAT_TYPE_M16N8K16_FLOAT => HmmaSize::M16N8K16,
                            val => unreachable!("unsupported HMMA type: {val}"),
                        };

                        assert_eq!(flags.a_type(), GLSL_TYPE_FLOAT16);
                        assert_eq!(flags.b_type(), GLSL_TYPE_FLOAT16);
                        assert!(!flags.sat());
                        b.push_op(OpHmma {
                            dst: dst.clone().into(),
                            dst_type: dst_type,
                            src_type: FloatType::F16,
                            mat_size: mat_size,
                            srcs: [cmat_a.into(), cmat_b.into(), cmat_c.into()],
                        });
                    }
                    NAK_CMAT_TYPE_M8N8K16_INT
                    | NAK_CMAT_TYPE_M16N8K16_INT
                    | NAK_CMAT_TYPE_M16N8K32_INT => {
                        let a_type = match flags.a_type() {
                            GLSL_TYPE_UINT8 => IntType::U8,
                            GLSL_TYPE_INT8 => IntType::I8,
                            val => unreachable!("Invalid a_type: {val}"),
                        };
                        let b_type = match flags.b_type() {
                            GLSL_TYPE_UINT8 => IntType::U8,
                            GLSL_TYPE_INT8 => IntType::I8,
                            val => unreachable!("Invalid b_type: {val}"),
                        };

                        let mat_size = match flags.cmat_type() {
                            NAK_CMAT_TYPE_M8N8K16_INT => ImmaSize::M8N8K16,
                            NAK_CMAT_TYPE_M16N8K16_INT => ImmaSize::M16N8K16,
                            NAK_CMAT_TYPE_M16N8K32_INT => ImmaSize::M16N8K32,
                            val => unreachable!("unsupported IMMA type: {val}"),
                        };

                        b.push_op(OpImma {
                            dst: dst.clone().into(),
                            mat_size,
                            srcs: [cmat_a.into(), cmat_b.into(), cmat_c.into()],
                            src_types: [a_type, b_type],
                            saturate: flags.sat(),
                        });
                    }
                    val => unreachable!("Unknown cmat_type {val}"),
                }

                self.set_dst(&intrin.def, dst.into());
            }
            _ => panic!(
                "Unsupported intrinsic instruction: {}",
                intrin.info().name()
            ),
        }
    }

    fn parse_load_const(
        &mut self,
        b: &mut impl SSABuilder,
        load_const: &nir_load_const_instr,
    ) {
        let values = &load_const.values();

        let mut dst = Vec::new();
        match load_const.def.bit_size {
            1 => {
                for c in 0..load_const.def.num_components {
                    let imm_b1 = unsafe { values[usize::from(c)].b };
                    dst.push(b.copy(imm_b1.into()));
                }
            }
            8 => {
                for dw in 0..load_const.def.num_components.div_ceil(4) {
                    let mut imm_u32 = 0;
                    for b in 0..4 {
                        let c = dw * 4 + b;
                        if c < load_const.def.num_components {
                            let imm_u8 = unsafe { values[usize::from(c)].u8_ };
                            imm_u32 |= u32::from(imm_u8) << (b * 8);
                        }
                    }
                    dst.push(b.copy(imm_u32.into()));
                }
            }
            16 => {
                for dw in 0..load_const.def.num_components.div_ceil(2) {
                    let mut imm_u32 = 0;
                    for w in 0..2 {
                        let c = dw * 2 + w;
                        if c < load_const.def.num_components {
                            let imm_u16 =
                                unsafe { values[usize::from(c)].u16_ };
                            imm_u32 |= u32::from(imm_u16) << (w * 16);
                        }
                    }
                    dst.push(b.copy(imm_u32.into()));
                }
            }
            32 => {
                for c in 0..load_const.def.num_components {
                    let imm_u32 = unsafe { values[usize::from(c)].u32_ };
                    dst.push(b.copy(imm_u32.into()));
                }
            }
            64 => {
                for c in 0..load_const.def.num_components {
                    let imm_u64 = unsafe { values[c as usize].u64_ };
                    dst.push(b.copy((imm_u64 as u32).into()));
                    dst.push(b.copy(((imm_u64 >> 32) as u32).into()));
                }
            }
            _ => panic!("Unknown bit size: {}", load_const.def.bit_size),
        }

        self.set_ssa(&load_const.def, dst);
    }

    fn parse_undef(
        &mut self,
        b: &mut impl SSABuilder,
        undef: &nir_undef_instr,
    ) {
        let dst = alloc_ssa_for_nir(b, &undef.def);
        for c in &dst {
            b.push_op(OpUndef { dst: (*c).into() });
        }
        self.set_ssa(&undef.def, dst);
    }

    fn emit_jump(
        &mut self,
        b: &mut impl SSABuilder,
        nb: &nir_block,
        target: &nir_block,
    ) {
        if target.index == self.end_block_id {
            b.push_op(OpExit {});
        } else {
            self.cfg.add_edge(nb.index, target.index);
            let target_label = self.get_block_label(target);

            match self.peek_crs(target) {
                Some(SyncType::Sync) => {
                    b.push_op(OpSync {
                        target: target_label,
                    });
                }
                Some(SyncType::Brk) => {
                    b.push_op(OpBrk {
                        target: target_label,
                    });
                }
                Some(SyncType::Cont) => {
                    b.push_op(OpCont {
                        target: target_label,
                    });
                }
                None => {
                    b.push_op(OpBra {
                        target: target_label,
                    });
                }
            }
        }
    }

    fn emit_pred_jump(
        &mut self,
        b: &mut impl SSABuilder,
        nb: &nir_block,
        pred: Pred,
        target: &nir_block,
        fallthrough: &nir_block,
    ) {
        // The fall-through edge has to come first
        self.cfg.add_edge(nb.index, fallthrough.index);
        let op = if target.index == self.end_block_id {
            Op::Exit(OpExit {})
        } else {
            self.cfg.add_edge(nb.index, target.index);
            Op::Bra(OpBra {
                target: self.get_block_label(target),
            })
        };
        b.predicate(pred).push_op(op);
    }

    fn parse_block(
        &mut self,
        ssa_alloc: &mut SSAValueAllocator,
        phi_map: &mut PhiAllocMap,
        nb: &nir_block,
    ) {
        let sm = self.sm;
        let mut b = SSAInstrBuilder::new(sm, ssa_alloc);

        if self.sm.sm() >= 70 && nb.index == 0 && self.nir.info.shared_size > 0
        {
            // The blob seems to always do a BSYNC before accessing shared
            // memory.  Perhaps this is to ensure that our allocation is
            // actually available and not in use by another thread?
            let label = self.label_alloc.alloc();
            let bar_clear = b.alloc_ssa(RegFile::Bar);

            b.push_op(OpBClear {
                dst: bar_clear.into(),
            });

            let bar = b.alloc_ssa(RegFile::Bar);
            b.push_op(OpBSSy {
                bar_out: bar.into(),
                bar_in: bar_clear.into(),
                cond: SrcRef::True.into(),
                target: label,
            });

            b.push_op(OpBSync {
                bar: bar.into(),
                cond: SrcRef::True.into(),
            });

            b.push_op(OpNop { label: Some(label) });
        }

        let mut phi = OpPhiDsts::new();
        for ni in nb.iter_instr_list() {
            let Some(np) = ni.as_phi() else {
                break;
            };

            if DEBUG.annotate() {
                let annotation = self
                    .nir_instr_printer
                    .instr_to_string(ni)
                    .unwrap()
                    .split_whitespace()
                    .collect::<Vec<_>>()
                    .join(" ");
                b.push_op(OpAnnotate {
                    annotation: format!("generated by \"{}\"", annotation,),
                });
            }

            let uniform = !nb.divergent
                && self.sm.num_regs(RegFile::UGPR) > 0
                && !DEBUG.no_ugpr()
                && !np.def.divergent;

            // This should be ensured by nak_nir_lower_cf()
            if uniform {
                for ps in np.iter_srcs() {
                    assert!(!ps.pred().divergent);
                }
            }

            let mut b = UniformBuilder::new(&mut b, uniform);
            let dst = alloc_ssa_for_nir(&mut b, np.def.as_def());
            for i in 0..dst.len() {
                let phi_id = phi_map.get_phi(np, i.try_into().unwrap());
                phi.dsts.push(phi_id, dst[i].into());
            }
            self.set_ssa(np.def.as_def(), dst);
        }

        if !phi.dsts.is_empty() {
            b.push_op(phi);
        }

        if self.sm.sm() < 70
            && nb.cf_node.prev().is_none()
            && nb.parent().as_loop().is_some()
        {
            b.push_op(OpPCnt {
                target: self.get_block_label(nb),
            });
            self.push_crs(nb, SyncType::Cont);
        }

        let mut goto = None;
        for ni in nb.iter_instr_list() {
            if DEBUG.annotate() && ni.type_ != nir_instr_type_phi {
                let annotation = self
                    .nir_instr_printer
                    .instr_to_string(ni)
                    .unwrap()
                    .split_whitespace()
                    .collect::<Vec<_>>()
                    .join(" ");
                b.push_op(OpAnnotate {
                    annotation: format!("generated by \"{}\"", annotation,),
                });
            }

            let uniform = !nb.divergent
                && self.sm.num_regs(RegFile::UGPR) > 0
                && !DEBUG.no_ugpr()
                && ni.def().is_some_and(|d| !d.divergent);
            let mut b = UniformBuilder::new(&mut b, uniform);

            match ni.type_ {
                nir_instr_type_alu => {
                    self.parse_alu(&mut b, ni.as_alu().unwrap())
                }
                nir_instr_type_jump => {
                    let jump = ni.as_jump().unwrap();
                    if jump.type_ == nir_jump_goto
                        || jump.type_ == nir_jump_goto_if
                    {
                        goto = Some(jump);
                    }
                }
                nir_instr_type_tex => {
                    self.parse_tex(&mut b, ni.as_tex().unwrap())
                }
                nir_instr_type_intrinsic => {
                    self.parse_intrinsic(&mut b, ni.as_intrinsic().unwrap())
                }
                nir_instr_type_load_const => {
                    self.parse_load_const(&mut b, ni.as_load_const().unwrap())
                }
                nir_instr_type_undef => {
                    self.parse_undef(&mut b, ni.as_undef().unwrap())
                }
                nir_instr_type_phi => (),
                _ => panic!("Unsupported instruction type"),
            }
        }

        if self.sm.sm() < 70 {
            if let Some(ni) = nb.following_if() {
                let fb = ni.following_block();
                b.push_op(OpSSy {
                    target: self.get_block_label(fb),
                });
                self.push_crs(fb, SyncType::Sync);
            } else if let Some(nl) = nb.following_loop() {
                let fb = nl.following_block();
                b.push_op(OpPBk {
                    target: self.get_block_label(fb),
                });
                self.push_crs(fb, SyncType::Brk);
            }
        }

        let succ = nb.successors();
        for sb in succ {
            let sb = match sb {
                Some(b) => b,
                None => continue,
            };

            let mut phi = OpPhiSrcs::new();

            for ni in sb.iter_instr_list() {
                let Some(np) = ni.as_phi() else {
                    break;
                };

                if DEBUG.annotate() {
                    let annotation = self
                        .nir_instr_printer
                        .instr_to_string(ni)
                        .unwrap()
                        .split_whitespace()
                        .collect::<Vec<_>>()
                        .join(" ");
                    b.push_op(OpAnnotate {
                        annotation: format!("generated by \"{}\"", annotation,),
                    });
                }

                for ps in np.iter_srcs() {
                    if ps.pred().index == nb.index {
                        let src = self.get_src(&ps.src);
                        let src = src.as_ssa().unwrap();
                        for (i, src) in src.iter().enumerate() {
                            let phi_id =
                                phi_map.get_phi(np, i.try_into().unwrap());
                            phi.srcs.push(phi_id, (*src).into());
                        }
                        break;
                    }
                }
            }

            if !phi.srcs.is_empty() {
                b.push_op(phi);
            }
        }

        if let Some(goto) = goto {
            let target = goto.target().unwrap();
            if goto.type_ == nir_jump_goto {
                self.emit_jump(&mut b, nb, target);
            } else {
                let cond = self.get_ssa(goto.condition.as_def())[0];
                let else_target = goto.else_target().unwrap();

                /* Next block in the NIR CF list */
                let next_block = nb.cf_node.next().unwrap().as_block().unwrap();

                if std::ptr::eq(else_target, next_block) {
                    self.emit_pred_jump(
                        &mut b,
                        nb,
                        // This is the branch to jump to the else
                        cond.into(),
                        target,
                        else_target,
                    );
                } else if std::ptr::eq(target, next_block) {
                    self.emit_pred_jump(
                        &mut b,
                        nb,
                        Pred::from(cond).bnot(),
                        else_target,
                        target,
                    );
                } else {
                    panic!(
                        "One of the two goto targets must be the next block in \
                            the NIR CF list"
                    );
                }
            }
        } else {
            if let Some(ni) = nb.following_if() {
                let cond = self.get_ssa(ni.condition.as_def())[0];
                self.emit_pred_jump(
                    &mut b,
                    nb,
                    // This is the branch to jump to the else
                    Pred::from(cond).bnot(),
                    ni.first_else_block(),
                    ni.first_then_block(),
                );
            } else {
                assert!(succ[1].is_none());
                let s0 = succ[0].unwrap();
                self.emit_jump(&mut b, nb, s0);
            }
        }

        let bb = BasicBlock {
            label: self.get_block_label(nb),
            uniform: !nb.divergent,
            instrs: b.into_vec(),
        };
        self.cfg.add_node(nb.index, bb);
    }

    fn parse_if(
        &mut self,
        ssa_alloc: &mut SSAValueAllocator,
        phi_map: &mut PhiAllocMap,
        ni: &nir_if,
    ) {
        self.parse_cf_list(ssa_alloc, phi_map, ni.iter_then_list());
        self.parse_cf_list(ssa_alloc, phi_map, ni.iter_else_list());

        if self.sm.sm() < 70 {
            let next_block = ni.cf_node.next().unwrap().as_block().unwrap();
            self.pop_crs(next_block, SyncType::Sync);
        }
    }

    fn parse_loop(
        &mut self,
        ssa_alloc: &mut SSAValueAllocator,
        phi_map: &mut PhiAllocMap,
        nl: &nir_loop,
    ) {
        self.parse_cf_list(ssa_alloc, phi_map, nl.iter_body());

        if self.sm.sm() < 70 {
            let header = nl.iter_body().next().unwrap().as_block().unwrap();
            self.pop_crs(header, SyncType::Cont);
            let next_block = nl.cf_node.next().unwrap().as_block().unwrap();
            self.pop_crs(next_block, SyncType::Brk);
        }
    }

    fn parse_cf_list(
        &mut self,
        ssa_alloc: &mut SSAValueAllocator,
        phi_map: &mut PhiAllocMap,
        list: ExecListIter<nir_cf_node>,
    ) {
        for node in list {
            match node.type_ {
                nir_cf_node_block => {
                    let nb = node.as_block().unwrap();
                    self.parse_block(ssa_alloc, phi_map, nb);
                }
                nir_cf_node_if => {
                    let ni = node.as_if().unwrap();
                    self.parse_if(ssa_alloc, phi_map, ni);
                }
                nir_cf_node_loop => {
                    let nl = node.as_loop().unwrap();
                    self.parse_loop(ssa_alloc, phi_map, nl);
                }
                _ => panic!("Invalid inner CF node type"),
            }
        }
    }

    pub fn parse_function_impl(&mut self, nfi: &nir_function_impl) -> Function {
        let mut ssa_alloc = SSAValueAllocator::new();
        let end_nb = nfi.end_block();
        self.end_block_id = end_nb.index;

        let mut phi_alloc = PhiAllocator::new();
        let mut phi_map = PhiAllocMap::new(&mut phi_alloc);

        self.parse_cf_list(&mut ssa_alloc, &mut phi_map, nfi.iter_body());

        let cfg = std::mem::take(&mut self.cfg).as_cfg();
        assert!(cfg.len() > 0);
        for i in 0..cfg.len() {
            if cfg[i].falls_through() {
                assert!(cfg.succ_indices(i)[0] == i + 1);
            }
        }

        let mut f = Function {
            ssa_alloc: ssa_alloc,
            phi_alloc: phi_alloc,
            blocks: cfg,
        };
        f.repair_ssa();
        f
    }

    pub fn parse_shader(mut self) -> Shader<'a> {
        let mut functions = Vec::new();
        for nf in self.nir.iter_functions() {
            if let Some(nfi) = nf.get_impl() {
                let f = self.parse_function_impl(nfi);
                functions.push(f);
            }
        }

        // Tessellation evaluation shaders MUST claim to read gl_TessCoord or
        // the hardware will throw an SPH error.
        if matches!(self.info.stage, ShaderStageInfo::Tessellation(_)) {
            match &mut self.info.io {
                ShaderIoInfo::Vtg(io) => {
                    let tc = NAK_ATTR_TESS_COORD;
                    io.mark_attrs_written(tc..(tc + 8));
                }
                _ => panic!("Tessellation must have ShaderIoInfo::Vtg"),
            }
        }

        Shader {
            sm: self.sm,
            info: self.info,
            functions: functions,
        }
    }
}

pub fn nak_shader_from_nir<'a>(
    nak: &nak_compiler,
    ns: &'a nir_shader,
    sm: &'a dyn ShaderModel,
) -> Shader<'a> {
    ShaderFromNir::new(nak, ns, sm).parse_shader()
}
