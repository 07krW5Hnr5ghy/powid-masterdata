package com.proyect.masterdata.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.Date;

@Builder
@AllArgsConstructor
@NoArgsConstructor
@Data
public class ShipmentDTO {
    private String purchase;
    private String warehouse;
    private String shipmentType;
    private Date registrationDate;
}
