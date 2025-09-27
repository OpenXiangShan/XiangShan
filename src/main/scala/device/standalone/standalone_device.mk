ifneq ($(DEVICE_PREFIX),)
RELEASE_ARGS += --xstop-prefix $(DEVICE_PREFIX)
DEBUG_ARGS += --xstop-prefix $(DEVICE_PREFIX)
PLDM_ARGS += --xstop-prefix $(DEVICE_PREFIX)
endif

STANDALONE_DEVICES = StandAlonePLIC StandAloneDebugModule StandAloneSYSCNT
STANDALONE_DEVICES_PATH = $(shell for module in $(STANDALONE_DEVICES); do echo $(RTL_DIR)/$$module/$(DEVICE_PREFIX)$$module.$(RTL_SUFFIX); done)

ifeq ($(DEVICE_TL),1)
DEVICE_ARGS += --use-tl
endif
ifeq ($(DEVICE_AXI4),1)
DEVICE_ARGS += --use-axi4
endif

ifneq ($(DEVICE_HART_NUM),)
NUM_CORES = $(DEVICE_HART_NUM)
endif

DEVICE_BASE_ADDR ?= -1
DEVICE_ADDR_WIDTH ?= -1
DEVICE_DATA_WIDTH ?= 64

$(STANDALONE_DEVICES): $(RTL_DIR)/$(GOALS)/$(DEVICE_PREFIX)$(GOALS).$(RTL_SUFFIX)

$(STANDALONE_DEVICES_PATH): $(SCALA_FILE)
	echo $@
	echo $(notdir $(@D))
	mkdir -p $(@D)
	$(TIME_CMD) mill -i xiangshan.runMain device.standalone.Main \
		-td $(@D) --num-cores $(NUM_CORES) $(RELEASE_ARGS) $(DEVICE_ARGS) \
		--standalone-device $(GOALS) --device-base-addr $(DEVICE_BASE_ADDR) \
		--device-addr-width $(DEVICE_ADDR_WIDTH) --device-data-width $(DEVICE_DATA_WIDTH)
	@git log -n 1 >> .__head__
	@git diff >> .__diff__
	@sed -i 's/^/\/\// ' .__head__
	@sed -i 's/^/\/\//' .__diff__
	@cat .__head__ .__diff__ $@ > .__out__
	@mv .__out__ $@
	@rm .__head__ .__diff__
