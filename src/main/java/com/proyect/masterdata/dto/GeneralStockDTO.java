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
public class GeneralStockDTO {
    private UUID id;
    private String user;
    private String product;
    private String productSku;
    private String categoryProduct;
    private String subCategoryProduct;
    private String model;
    private String color;
    private String size;
    private Integer quantity;
    private OffsetDateTime registrationDate;
    private OffsetDateTime updateDate;
}
