package com.proyect.masterdata.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.time.OffsetDateTime;
import java.util.UUID;

@Builder
@AllArgsConstructor
@NoArgsConstructor
@Data
public class KardexOutputDTO {
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
    private Double unitPrice;
    private OffsetDateTime registrationDate;
    private Double total;
    private String warehouse;
    private Long lotNumber;
    private Long orderNumber;
    private Double unitValue;
}
