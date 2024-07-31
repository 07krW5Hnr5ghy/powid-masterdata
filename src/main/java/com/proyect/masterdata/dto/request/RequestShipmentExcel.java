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
    private String shipmentDocument;
    private String warehouse;
    private String supplier;
    private String shipmentType;
    private String tokenUser;
}
