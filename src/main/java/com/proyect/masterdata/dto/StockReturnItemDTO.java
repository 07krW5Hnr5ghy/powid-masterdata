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
    private String serial;
    private String purchase;
    private String supplierProduct;
    private Integer quantity;
    private String observations;
    private Date registrationDate;
    private String supplier;
}
