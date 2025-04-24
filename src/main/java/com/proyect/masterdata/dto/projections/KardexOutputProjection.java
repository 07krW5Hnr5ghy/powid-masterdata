package com.proyect.masterdata.dto.projections;

import java.util.UUID;

public interface KardexOutputProjection {
   UUID getProductId();
   UUID getWarehouseId();
   Long getLotNumber();
}
