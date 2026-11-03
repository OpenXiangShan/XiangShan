from types import SimpleNamespace
from env.funcov.py.ifu import owner_v3_funcov as owner


def test_ordinary_ibuffer_end_metadata_does_not_imply_registered_writeback(monkeypatch):
    marks=[]
    monkeypatch.setattr(owner,"mark_owner_v3_checked",lambda recorder,bin_id,*args,**kwargs: marks.append(bin_id))
    recorder=SimpleNamespace(hit_count_by_bin_id=lambda _bin_id:1)
    owner.derive_owner_v3_from_source(recorder,"BIN-836",23,{"ordinary_end_metadata":True})
    assert "BIN-944" not in marks
    assert owner.OWNER_V3_BIN_BY_ID["BIN-944"].bin_name == "owner_leaf_046"
    owner.derive_owner_v3_from_source(recorder,"BIN-832",24,{})
    assert "BIN-966" in marks
