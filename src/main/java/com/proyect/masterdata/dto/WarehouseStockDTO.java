package com.proyect.masterdata.dto;

import java.time.OffsetDateTime;
import java.util.Date;
import java.util.UUID;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Builder
@AllArgsConstructor
@NoArgsConstructor
@Data
public class WarehouseStockDTO {
    public UUID id;
    public String user;
    private String product;
    private String productSku;
    private String categoryProduct;
    private String subCategoryProduct;
    public String warehouse;
    public String model;
    public String color;
    public String size;
    public Integer quantity;
    public OffsetDateTime registrationDate;
    public OffsetDateTime updateDate;
}
