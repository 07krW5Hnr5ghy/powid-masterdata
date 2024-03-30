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
public class StockReturnItemDTO {
    private String purchaseSerial;
    private String supplierProductSerial;
    private Integer quantity;
    private String observations;
    private Date registrationDate;
    private String supplier;
    private Long id;
}
