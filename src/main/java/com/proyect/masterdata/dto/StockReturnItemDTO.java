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
    private String supplierProduct;
    private String productSku;
    private String model;
    private String color;
    private String size;
    private Integer quantity;
    private String observations;
    private Date registrationDate;
    private String supplier;
    private String warehouse;
}
