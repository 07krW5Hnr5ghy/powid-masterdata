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
public class GeneralStockDTO {
    public String supplierProduct;
    public String productSku;
    public String model;
    public String color;
    public String size;
    public String supplier;
    public Integer quantity;
    public Date registrationDate;
    public Date updateDate;
}
