from types import SimpleNamespace

from env.model.branch_checker import BranchChecker


def test_branch_checker_counts_branch_types_and_mispredicts():
    checker = BranchChecker()
    checker.observe(
        SimpleNamespace(
            cycle=1,
            slot=0,
            pc=0x80000000,
            instr=0x00000063,
            is_rvc=False,
            pred_taken=False,
        )
    )
    checker.record_mispredict()

    stats = checker.get_stats()

    assert stats["total_branch"] == 1
    assert stats["mispredict"] == 1
