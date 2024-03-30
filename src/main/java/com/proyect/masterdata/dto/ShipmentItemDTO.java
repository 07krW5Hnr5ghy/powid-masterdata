package com.proyect.masterdata.dto;

import java.util.Date;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Builder
@AllArgsConstructor
@NoArgsConstructor
@Data
public class ShipmentItemDTO {
    private Long id;
    private String warehouse;
    private Integer quantity;
    private String supplierProductSerial;
    private String purchaseSerial;
    private Date registrationDate;
}
