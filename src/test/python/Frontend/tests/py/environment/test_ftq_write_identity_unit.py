import pytest
from env.support.ftq_write_identity import _POINTER_INPUTS, ftq_write_flag

@pytest.mark.parametrize('s3,s2,expected',[(0,0,(0,11)),(0,1,(1,22)),(1,0,(1,33)),(1,1,(1,33))])
def test_prediction_pointer_priority(s3,s2,expected):
    values=dict(s3_override=s3,s2_override=s2,bpu_flag=0,bpu_value=11,s2_flag=1,s2_value=22,s3_flag=1,s3_value=33)
    source={_POINTER_INPUTS[k]:v for k,v in values.items()}
    assert ftq_write_flag(source.get, expected[1])==expected[0]

def test_missing_or_inconsistent_identity_fails_closed():
    assert ftq_write_flag(lambda p: None,1) is None
    values={_POINTER_INPUTS[k]:v for k,v in dict(s3_override=0,s2_override=0,bpu_flag=1,bpu_value=3).items()}
    assert ftq_write_flag(values.get,4) is None
    values[_POINTER_INPUTS['bpu_flag']]=2
    assert ftq_write_flag(values.get,3) is None
