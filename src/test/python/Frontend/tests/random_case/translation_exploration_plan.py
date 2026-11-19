"""Opt-in translation model plans for reachability study, never runtime admission.

The existing long-program wrapper/defaults are unchanged. Stage/fault/context
plans here require separate new-source DUT, filter and recovery-oracle gates.
"""
from __future__ import annotations

from dataclasses import asdict, dataclass
import random
import hashlib
import json

from env.sequences import TranslationPte, TranslationScenario
try:
    from .translation_timing import _physical_permissions
except ImportError:
    from translation_timing import _physical_permissions


@dataclass(frozen=True)
class TranslationExplorationPlan:
    program_seed: int
    timing_seed: int
    mode: str = 'stage1'
    asid: int = 0
    asid_bits: int | None = None
    vmid: int = 0
    vmid_bits: int | None = None
    virtual_base: int = 0x80000000
    physical_base: int = 0x80400000
    guest_physical_base: int = 0x80600000
    s1_pbmt: int = 0
    s2_pbmt: int = 0
    pma_cacheable: bool = True
    fault: str = 'none'
    keep_pending_after_fence: bool = False
    ptw_ready_high: int = 1
    ptw_ready_low: int = 0

    def validate(self):
        for name in ('program_seed', 'timing_seed'):
            value = getattr(self, name)
            if type(value) is not int or not 0 <= value < 2**64:
                raise ValueError(name+' must be exact uint64')
        if self.mode not in ('stage1', 'only_stage1', 'only_stage2', 'all_stage'):
            raise ValueError('unsupported explicit translation mode')
        if type(self.asid) is not int or self.asid < 0:
            raise ValueError('ASID must be a nonnegative explicit integer')
        if self.asid != 0 and (type(self.asid_bits) is not int or not 1 <= self.asid_bits <= 16
                               or self.asid >= 2**self.asid_bits):
            raise ValueError('nonzero ASID requires reviewed width and normalized value')
        if type(self.vmid) is not int or self.vmid < 0:
            raise ValueError('VMID must be a nonnegative explicit integer')
        if self.mode in ('only_stage1', 'only_stage2', 'all_stage'):
            if type(self.vmid_bits) is not int or not 1 <= self.vmid_bits <= 16:
                raise ValueError('virtualized model plans require reviewed VMID width')
            if self.vmid >= 2**self.vmid_bits:
                raise ValueError('VMID is not already width-normalized; no silent truncation')
        if self.mode == 'stage1' and self.vmid != 0:
            raise ValueError('nonvirtual stage1 does not activate VMID')
        for name in ('virtual_base','physical_base','guest_physical_base'):
            value = getattr(self, name)
            if type(value) is not int or value < 0 or value % 4096:
                raise ValueError(name+' must be page aligned')
        if any(type(v) is not int or v not in (0,1,2) for v in (self.s1_pbmt,self.s2_pbmt)):
            raise ValueError('PBMT must be PMA/NC/IO')
        if self.mode in ('stage1','only_stage1') and self.s2_pbmt != 0:
            raise ValueError('stage2 PBMT requires active stage2')
        if self.mode == 'only_stage2' and self.s1_pbmt != 0:
            raise ValueError('onlyStage2 has no active stage1 PBMT')
        if self.fault not in ('none','s1_pf','s1_af','s2_gpf','s2_gaf'):
            raise ValueError('unsupported explicit response fault')
        if self.mode == 'only_stage2' and self.fault.startswith('s1_'):
            raise ValueError('stage1 fault requires active stage1')
        if self.mode in ('stage1','only_stage1') and self.fault.startswith('s2_'):
            raise ValueError('guest fault requires active stage2')
        for name in ('pma_cacheable','keep_pending_after_fence'):
            if type(getattr(self,name)) is not bool:
                raise ValueError(name+' must be explicit boolean')
        if (type(self.ptw_ready_high) is not int or self.ptw_ready_high < 1
                or type(self.ptw_ready_low) is not int or self.ptw_ready_low < 0):
            raise ValueError('ready pattern must permit bounded progress')

    def model_scenario(self, payload: bytes):
        """Construct existing public model inputs in memory; never generate a program."""
        self.validate()
        if not isinstance(payload, bytes) or not payload:
            raise ValueError('existing nonempty binary bytes required')
        mode = {'stage1':0,'only_stage1':1,'only_stage2':2,'all_stage':3}[self.mode]
        rng = random.Random(self.timing_seed ^ 0xC242B00D5)
        latency = rng.choice((1,3,8,16))
        pages = (len(payload)+4095)//4096
        pmp,pma = _physical_permissions(self.physical_base,pages,
                         pma_pattern=(self.pma_cacheable,),layout='exact_napot_v1')
        pbmt = self.s1_pbmt if self.mode!='only_stage2' and self.s1_pbmt else self.s2_pbmt
        faulting = self.fault != 'none'
        identity = hashlib.sha256(json.dumps({'plan':asdict(self),
                 'payload_sha256':hashlib.sha256(payload).hexdigest()},sort_keys=True,separators=(',', ':')).encode()).hexdigest()
        return TranslationScenario(
            scenario_id=f'translation-model-s{self.program_seed:016x}-t{self.timing_seed:016x}-{identity}',
            va=self.virtual_base,pa=self.physical_base,payload=payload,page_count=pages,
            mode='bare' if mode==2 else 'sv39',stage2_mode='sv39' if mode in (2,3) else None,
            gpa=self.guest_physical_base if mode==3 else None,s2xlate=mode,priv_virt=int(mode!=0),priv_imode=1,
            satp_asid=self.asid if mode==0 else 0,vsatp_asid=self.asid if mode in (1,3) else 0,
            hgatp_vmid=self.vmid if mode in (2,3) else 0,
            s1_pte=TranslationPte(asid=self.asid,vmid=self.vmid if mode==3 else 0,pbmt=self.s1_pbmt),
            s2_pte=TranslationPte(vmid=self.vmid,pbmt=self.s2_pbmt),
            s1_pf=int(self.fault=='s1_pf'),s1_af=int(self.fault=='s1_af'),
            s2_gpf=int(self.fault=='s2_gpf'),s2_gaf=int(self.fault=='s2_gaf'),
            ptw_machine_pbmte=1,ptw_hypervisor_pbmte=1,
            ptw_response_latency=latency,ptw_response_latency_max=latency+3,
            ptw_response_seed=self.timing_seed,
            ptw_req_ready_strategy='periodic' if self.ptw_ready_low else 'always',
            ptw_req_ready_high_cycles=self.ptw_ready_high,ptw_req_ready_low_cycles=self.ptw_ready_low,
            ptw_flush_pending_on_sfence=not self.keep_pending_after_fence,
            pmp_entries=pmp,pma_entries=pma,
            expected_path='fault' if faulting else 'uncache' if pbmt or not self.pma_cacheable else 'cacheable',
            expected_result='guest_fault' if self.fault=='s2_gpf' else 'page_fault' if self.fault=='s1_pf'
                else 'access_fault' if faulting else 'normal')

    def admission(self):
        self.validate()
        return {'schema_version':1,'status':'MODEL_ONLY','plan':asdict(self),
                'plan_sha256':hashlib.sha256(json.dumps(asdict(self),sort_keys=True,separators=(',', ':')).encode()).hexdigest(),
                'long_golden_trace_admitted':False,'canonical_hit_eligible':False,
                'runtime_reachability':'UNCONFIRMED',
                'input_hash_binding':'not supplied; model-only plans cannot replace five original input hashes',
                'missing':['new c242 source/build/signal/sampler/registry/environment contract',
                           'actual native request stage/getGpa/context identity and PTWFilter boundary',
                           'late response valid/ready and owner reject evidence after fence',
                           'no old committed side effect, correct exception/GPA and recovery PC/progress',
                           'full input/scenario/timeline hashes and first-hit/cost measurements']}
