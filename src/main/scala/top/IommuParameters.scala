/***************************************************************************************
 * Copyright (c) 2026 Beijing Institute of Open Source Chip (BOSC)
 *
 * XiangShan is licensed under Mulan PSL v2.
 ***************************************************************************************/

package top

import org.chipsalliance.cde.config.Field

/** Controls instantiation of the external bosc IOMMU simulation model. */
case object EnableIommuKey extends Field[Boolean](false)
