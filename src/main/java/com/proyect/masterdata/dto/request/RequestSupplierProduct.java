package com.proyect.masterdata.dto.request;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Builder
@AllArgsConstructor
@NoArgsConstructor
@Data
public class RequestSupplierProduct {
    private String serial;
    private String productSku;
    private String supplierRuc;
    private Double purchasePrice;
}
