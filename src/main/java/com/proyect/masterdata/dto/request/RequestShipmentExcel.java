package com.proyect.masterdata.dto.request;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Builder
@AllArgsConstructor
@NoArgsConstructor
@Data
public class RequestShipmentExcel {
    private String purchaseSerial;
    private String warehouse;
    private String shipmentType;
    private String tokenUser;
}
