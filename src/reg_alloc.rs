use std::collections::{HashMap, HashSet};

use crate::{
    ir::{Inst, InstLoc, IrSnippet, Value, ValueInner, VregId},
    types::Type,
};

pub fn alloc(ir: &IrSnippet) -> Result<RegMap, RegAllocError> {
    let mut liveness_ranges: HashMap<VregId, LivenessRange> = HashMap::new();
    let mut vreg_types: HashMap<VregId, Type> = HashMap::new();

    let mut seen_vreg = |vreg: VregId, pos: InstLoc| {
        let range = liveness_ranges
            .entry(vreg)
            .or_insert_with(|| LivenessRange {
                start: pos,
                end: pos,
            });
        range.end = pos;
    };

    for (loc, inst) in ir.insts.iter().enumerate() {
        for vreg in inst.touches_vregs() {
            seen_vreg(vreg, loc);
        }

        if let Some(Value {
            type_,
            inner: ValueInner::Vreg(vreg),
        }) = &inst.assigns
        {
            vreg_types.insert(*vreg, type_.clone());
        }
    }

    let mut graph: HashMap<VregId, GraphNode> = HashMap::new();

    for (i, i_r) in liveness_ranges.iter() {
        let _ = graph.entry(*i).or_insert_with(|| GraphNode::new());

        if i_r.start == i_r.end {
            continue;
        }

        for (j, j_r) in liveness_ranges.iter() {
            if i == j {
                continue;
            }

            if j_r.start == j_r.end {
                continue;
            }

            if range_intersects(*i_r, *j_r) {
                let node = graph.entry(*i).or_insert_with(|| GraphNode::new());
                node.neighbours.insert(*j);

                let node = graph.entry(*j).or_insert_with(|| GraphNode::new());
                node.neighbours.insert(*j);
            }
        }
    }

    let mut map = RegMap::new();

    for (vreg, range) in liveness_ranges.iter() {
        if range.start == range.end {
            continue;
        }

        let Some(pool) = vreg_types.get(vreg).and_then(|t| t.pool()) else {
            continue;
        };

        let mut cant_use = arrayvec::ArrayVec::<usize, 32>::new();

        for n in &graph.get(vreg).as_ref().unwrap().neighbours {
            if let Some((n_pool, index)) = map.allocations.get(n) {
                if pool != *n_pool {
                    continue;
                }

                cant_use.push(*index);
            }
        }

        for i in 0..32 {
            if !cant_use.contains(&i) {
                map.allocations.insert(*vreg, (pool, i));
                break;
            }
        }
    }

    Ok(map)
}

#[derive(Copy, Clone, Debug)]
struct LivenessRange {
    start: InstLoc,
    end: InstLoc,
}

struct GraphNode {
    neighbours: HashSet<VregId>,
}

impl GraphNode {
    fn new() -> Self {
        Self {
            neighbours: HashSet::new(),
        }
    }
}

#[derive(Clone, Debug)]
pub struct RegMap {
    allocations: HashMap<VregId, (PoolId, usize)>,
}

impl RegMap {
    fn new() -> Self {
        Self {
            allocations: HashMap::new(),
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum RegType {
    Int,
    Float,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct PoolId {
    pub reg_type: RegType,
    pub bits: usize,
}

#[derive(Clone, Debug)]
pub enum RegAllocError {
    Misc,
}

impl Inst {
    fn touches_vregs(&self) -> Vec<VregId> {
        let mut touches = vec![];

        if let Some(assings) = &self.assigns {
            if let Some(vreg) = assings.as_vreg() {
                touches.push(vreg);
            }
        }

        let vals = match &self.inner {
            crate::ir::InstInner::BinOp { lhs, rhs, .. } => {
                vec![lhs, rhs]
            }
            crate::ir::InstInner::Call { args, .. } => args.iter().collect::<Vec<_>>(),
            crate::ir::InstInner::Label(_) => vec![],
            crate::ir::InstInner::Jz(_, value) => vec![value],
            crate::ir::InstInner::Jmp(_) => vec![],
            crate::ir::InstInner::Load(value) => vec![value],
            crate::ir::InstInner::Store(value, value1) => vec![value, value1],
            crate::ir::InstInner::Alloca => vec![],
            crate::ir::InstInner::Function { .. } => vec![],
            crate::ir::InstInner::Ret(value) => vec![value],
            crate::ir::InstInner::Cast(value) => vec![value],
        };

        touches.extend(vals.into_iter().filter_map(|v| v.as_vreg()));

        touches
    }
}

fn range_intersects(a: LivenessRange, b: LivenessRange) -> bool {
    (a.start >= b.start && a.start <= b.end) || (b.start >= a.start && b.start <= a.end)
}

impl Type {
    fn pool(&self) -> Option<PoolId> {
        Some(match self {
            Type::F64 => PoolId {
                reg_type: RegType::Float,
                bits: 64,
            },
            Type::Ptr(_) => PoolId {
                reg_type: RegType::Int,
                bits: 64,
            },
            _ => return None,
        })
    }
}
