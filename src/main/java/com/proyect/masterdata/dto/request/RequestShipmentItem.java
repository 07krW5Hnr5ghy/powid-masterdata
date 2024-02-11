package com.proyect.masterdata.dto.request;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Builder
@AllArgsConstructor
@NoArgsConstructor
@Data
public class RequestShipmentItem {
    private Integer quantity;
    private String observations;
    private String supplierProductSerial;
    private String purchaseSerial;
}
