package com.proyect.masterdata.dto.request;

import com.proyect.masterdata.domain.Product;
import com.proyect.masterdata.domain.SupplyOrderItem;
import com.proyect.masterdata.domain.Warehouse;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.UUID;

@Builder
@AllArgsConstructor
@NoArgsConstructor
@Data
public class RequestKardexOutput {
    private String user;
    private Integer quantity;
    private Product product;
    private Warehouse warehouse;
    private Long orderNumber;
    private UUID deliveryManifestItemId;
}
