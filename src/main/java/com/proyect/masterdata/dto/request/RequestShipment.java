package com.proyect.masterdata.dto.request;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Builder
@AllArgsConstructor
@NoArgsConstructor
@Data
public class RequestShipment {
    private String serial;
    private Integer quantity;
    private Double totalPurchasePrice;
    private Double unitPurchasePrice;
    private String observations;
    private String supplierProductSerial;
    private String purchaseSerial;
}
