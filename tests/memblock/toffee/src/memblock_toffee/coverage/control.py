from toffee.funcov import CovGroup, Eq

from memblock_toffee.env.control import ControlComponent


def control_coverage(component: ControlComponent) -> CovGroup:
    group = CovGroup("TOFFEE_CONTROL")
    group.add_watch_point(
        component.bundle.l2_flush_enable,
        {"disabled": Eq(0), "enabled": Eq(1)},
        name="l2_flush_enable",
    )
    group.add_watch_point(
        component.bundle.l2_flush_done,
        {"incomplete": Eq(0), "complete": Eq(1)},
        name="l2_flush_done",
    )
    group.add_watch_point(
        component.bundle.outer_l2_flush_enable,
        {"disabled": Eq(0), "enabled": Eq(1)},
        name="outer_l2_flush_enable",
    )
    group.add_watch_point(
        component.bundle.backend_l2_flush_done,
        {"incomplete": Eq(0), "complete": Eq(1)},
        name="backend_l2_flush_done",
    )
    return group
