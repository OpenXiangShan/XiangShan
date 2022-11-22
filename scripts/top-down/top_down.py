import csv
import sys
from pyecharts.charts import Page, Sunburst
from pyecharts import options as opts


class TopDown:
    """TopDown node"""
    def __init__(self, name, percentage):
        self.name = name
        if isinstance(percentage, TopDown):
            self.percentage = percentage.percentage
        else:
            self.percentage = percentage
        self.down = {}
        self.top = None
        self.level = 0

    def __add__(self, rhs):
        if isinstance(rhs, TopDown):
            return self.percentage + rhs.percentage
        return self.percentage + rhs

    def __radd__(self, lhs):
        if isinstance(lhs, TopDown):
            return lhs.percentage + self.percentage
        return lhs + self.percentage

    def __sub__(self, rhs):
        if isinstance(rhs, TopDown):
            return self.percentage - rhs.percentage
        return self.percentage - rhs

    def __rsub__(self, lhs):
        if isinstance(lhs, TopDown):
            return lhs.percentage - self.percentage
        return lhs - self.percentage

    def __mul__(self, rhs):
        if isinstance(rhs, TopDown):
            return self.percentage * rhs.percentage
        return self.percentage * rhs

    def __rmul__(self, lhs):
        if isinstance(lhs, TopDown):
            return lhs.percentage * self.percentage
        return lhs * self.percentage

    def __truediv__(self, rhs):
        if isinstance(rhs, TopDown):
            return self.percentage / rhs.percentage
        return self.percentage / rhs

    def __rtruediv__(self, lhs):
        if isinstance(lhs, TopDown):
            return lhs.percentage / self.percentage
        return lhs / self.percentage

    def add_down(self, name, percentage):
        """Add a leaf node

        Args:
            name (str): Name of leaf node
            percentage (float): Percentage of leaf node

        Returns:
            TopDown: leaf
        """
        self.down[name] = TopDown(name, percentage)
        self.down[name].top = self
        self.down[name].level = self.level + 1
        return self.down[name]

    def draw(self):
        """Draw the TopDown sunburst chart

        Returns:
            _type_: _description_
        """
        if not self.down:
            return [opts.SunburstItem(name=self.name, value=self.percentage)]
        items = []
        for value in self.down.values():
            items.append(value.draw()[0])
        if self.top:
            return [opts.SunburstItem(name=self.name, value=self.percentage, children=items)]
        return items


def process_one(path, head):
    """Process one chart

    Args:
        path (String): csv path
        head (String): chart head

    Returns:
        Sunburst chart
    """
    with open(path, encoding='UTF-8') as file:
        csv_file = dict(csv.reader(file))

    def use(name):
        return float(csv_file[name])

    csv_file['total_slots'] = use('total_cycles') * 6
    csv_file['ifu2id_allNO_slots'] = use('ifu2id_allNO_cycle') * 6
    csv_file['ifu2id_hvButNotFull_slots'] = use('fetch_bubbles') - use('ifu2id_allNO_slots')

    stall_cycles_core = use('stall_cycle_fp') + use('stall_cycle_int') + use('stall_cycle_rob') + use('stall_cycle_int_dq') + use('stall_cycle_fp_dq') + use('ls_dq_bound_cycles')

    top = TopDown("Top", 1.0)

# top
    frontend_bound = top.add_down("Frontend Bound", use('decode_bubbles') / use('total_slots'))
    bad_speculation = top.add_down("Bad Speculation", (use('slots_issued') - use('slots_retired') + use('recovery_bubbles')) / use('total_slots'))
    retiring = top.add_down("Retiring", use('slots_retired') / use('total_slots'))
    backend_bound = top.add_down("Backend Bound", top - frontend_bound - bad_speculation - retiring)

#top->frontend_bound
    fetch_latency = frontend_bound.add_down("Fetch Latency", use('fetch_bubbles') / use('total_slots'))
    fetch_bandwidth = frontend_bound.add_down("Fetch Bandwidth", frontend_bound - fetch_latency)

# top->frontend_bound->fetch_latency
    itlb_miss = fetch_latency.add_down("iTLB Miss", use('itlb_miss_cycles') / use('total_cycles'))
    icache_miss = fetch_latency.add_down("iCache Miss", use('icache_miss_cycles') / use('total_cycles'))
    stage2_redirect_cycles = fetch_latency.add_down("Stage2 Redirect", use('stage2_redirect_cycles') / use('total_cycles'))
    if2id_bandwidth = fetch_latency.add_down("IF2ID Bandwidth", use('ifu2id_hvButNotFull_slots') / use('total_slots'))
    fetch_latency_others = fetch_latency.add_down("Fetch Latency Others", fetch_latency - itlb_miss - icache_miss - stage2_redirect_cycles - if2id_bandwidth)

# top->frontend_bound->fetch_latency->stage2_redirect_cycles
    branch_resteers = stage2_redirect_cycles.add_down("Branch Resteers", use('branch_resteers_cycles') / use('total_cycles'))
    robFlush_bubble = stage2_redirect_cycles.add_down("RobFlush Bubble", use('robFlush_bubble_cycles') / use('total_cycles'))
    ldReplay_bubble = stage2_redirect_cycles.add_down("LdReplay Bubble", use('ldReplay_bubble_cycles') / use('total_cycles'))

# top->bad_speculation
    branch_mispredicts = bad_speculation.add_down("Branch Mispredicts", bad_speculation)

# top->backend_bound
    memory_bound = backend_bound.add_down("Memory Bound", backend_bound * (use('store_bound_cycles') + use('load_bound_cycles')) / (
        stall_cycles_core + use('store_bound_cycles') + use('load_bound_cycles')))
    core_bound = backend_bound.add_down("Core Bound", backend_bound - memory_bound)

# top->backend_bound->memory_bound
    stores_bound = memory_bound.add_down("Stores Bound", use('store_bound_cycles') / use('total_cycles'))
    loads_bound = memory_bound.add_down("Loads Bound", use('load_bound_cycles') / use('total_cycles'))

# top->backend_bound->core_bound
    integer_dq = core_bound.add_down("Integer DQ", core_bound * use('stall_cycle_int_dq') / stall_cycles_core)
    floatpoint_dq = core_bound.add_down("Floatpoint DQ", core_bound * use('stall_cycle_fp_dq') / stall_cycles_core)
    rob = core_bound.add_down("ROB", core_bound * use('stall_cycle_rob') / stall_cycles_core)
    integer_prf = core_bound.add_down("Integer PRF", core_bound * use('stall_cycle_int') / stall_cycles_core)
    floatpoint_prf = core_bound.add_down("Floatpoint PRF", core_bound * use('stall_cycle_fp') / stall_cycles_core)
    lsu_ports = core_bound.add_down("LSU Ports", core_bound * use('ls_dq_bound_cycles') / stall_cycles_core)

# top->backend_bound->memory_bound->loads_bound
    l1d_loads_bound = loads_bound.add_down("L1D Loads", use('l1d_loads_bound_cycles') / use('total_cycles'))
    l2_loads_bound = loads_bound.add_down("L2 Loads", use('l2_loads_bound_cycles') / use('total_cycles'))
    l3_loads_bound = loads_bound.add_down("L3 Loads", use('l3_loads_bound_cycles') / use('total_cycles'))
    ddr_loads_bound = loads_bound.add_down("DDR Loads", use('ddr_loads_bound_cycles') / use('total_cycles'))

# top->backend_bound->memory_bound->loads_bound->l1d_loads_bound
    l1d_loads_mshr_bound = l1d_loads_bound.add_down("L1D Loads MSHR", use('l1d_loads_mshr_bound') / use('total_cycles'))
    l1d_loads_tlb_bound = l1d_loads_bound.add_down("L1D Loads TLB", use('l1d_loads_tlb_bound') / use('total_cycles'))
    l1d_loads_store_data_bound = l1d_loads_bound.add_down("L1D Loads sdata", use('l1d_loads_store_data_bound') / use('total_cycles'))
    l1d_loads_bank_conflict_bound = l1d_loads_bound.add_down("L1D Loads\nBank Conflict", use('l1d_loads_bank_conflict_bound') / use('total_cycles'))
    l1d_loads_vio_check_redo_bound = l1d_loads_bound.add_down("L1D Loads VioRedo", use('l1d_loads_vio_check_redo_bound') / use('total_cycles'))


    return (
        Sunburst(init_opts=opts.InitOpts(width="1000px", height="1200px"))
        .add(series_name="", data_pair=top.draw(), radius=[0, "90%"])
        .set_global_opts(title_opts=opts.TitleOpts(title=head))
        .set_series_opts(label_opts=opts.LabelOpts(formatter="{b}")))


title = sys.argv[1]
directory = sys.argv[2]
suffix = sys.argv[3]
print(title)
(
    Page(page_title=title, layout=Page.SimplePageLayout)
    .add(process_one(directory + "/csv/" + title + ".log.csv", title + "_" + suffix))
    .render(directory + "/html/" + title + ".html"))
