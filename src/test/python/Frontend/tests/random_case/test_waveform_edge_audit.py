from __future__ import annotations

from io import StringIO

from .waveform_edge_audit import iter_rising_edge_snapshots


def test_rising_edge_snapshot_uses_final_values_at_timestamp() -> None:
    waveform = StringIO(
        """$timescale 1ps $end
$scope module TOP $end
$scope module Frontend_top $end
$var logic 1 ! clock $end
$var logic 1 \" io_backend_cfVec_0_valid $end
$var logic 50 # io_backend_cfVec_0_bits_pc [49:0] $end
$var logic 6 $ io_backend_cfVec_0_bits_ftqPtr_value [5:0] $end
$var logic 1 % io_backend_toFtq_redirect_valid $end
$var logic 51 & io_backend_toFtq_redirect_bits_target [50:0] $end
$var logic 1 ' io_backend_fromFtq_wen $end
$var logic 6 ( io_backend_fromFtq_ftqIdx [5:0] $end
$var logic 50 ) io_backend_fromFtq_startPc_addr [49:0] $end
$upscope $end
$upscope $end
$enddefinitions $end
#0
0!
0\"
0%
b0 #
b0 $
b0 &
0'
b0 (
b0 )
#1
1!
b10000000000000000010111111111110 #
b100101 $
1\"
b10000000000000000010111111111110 &
1%
1'
b100101 (
b1000000000000000001011111111111 )
#2
0!
#3
1!
b10000000000000000011000000001010 #
b100111 $
0%
#4
0!
"""
    )

    snapshots = list(
        iter_rising_edge_snapshots(waveform, start_time=1, end_time=3)
    )

    assert [snapshot["time"] for snapshot in snapshots] == [1, 3]
    assert snapshots[0]["redirect"]["valid"] == 1
    assert snapshots[0]["redirect"]["bits_target"] == 0x80002FFE
    assert snapshots[0]["cfvec"][0]["valid"] == 1
    assert snapshots[0]["cfvec"][0]["bits_pc"] == 0x80002FFE
    assert snapshots[0]["cfvec"][0]["bits_ftqPtr_value"] == 37
    assert snapshots[0]["from_ftq"]["wen"] == 1
    assert snapshots[0]["from_ftq"]["ftqIdx"] == 37
    assert snapshots[0]["from_ftq"]["start_pc"] == 0x80002FFE
    assert snapshots[1]["redirect"]["valid"] == 0
    assert snapshots[1]["cfvec"][0]["bits_pc"] == 0x8000300A


def test_unknown_values_are_preserved_as_none() -> None:
    waveform = StringIO(
        """$scope module TOP $end
$scope module Frontend_top $end
$var logic 1 ! clock $end
$var logic 1 \" io_backend_cfVec_0_valid $end
$var logic 50 # io_backend_cfVec_0_bits_pc [49:0] $end
$upscope $end
$upscope $end
$enddefinitions $end
#0
0!
0\"
bx #
#1
1!
1\"
"""
    )

    [snapshot] = list(iter_rising_edge_snapshots(waveform))

    assert snapshot["cfvec"][0]["valid"] == 1
    assert snapshot["cfvec"][0]["bits_pc"] is None
