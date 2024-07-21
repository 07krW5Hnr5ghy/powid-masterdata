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
    private String serial;
    private String shipmentSerial;
    private String shipmentDocument;
    private String warehouse;
    private String shipmentType;
    private String tokenUser;
}
