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
    private String warehouse;
    private Integer quantity;
    private String supplierProduct;
    private String purchase;
    private Date registrationDate;
}
