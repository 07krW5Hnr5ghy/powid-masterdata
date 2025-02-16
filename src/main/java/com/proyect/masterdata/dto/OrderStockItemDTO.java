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
public class OrderStockItemDTO {
    private UUID orderId;
    private String warehouse;
    private String supplierProduct;
    private String product;
    private String model;
    private String color;
    private String size;
    private Integer quantity;
    private OffsetDateTime registrationDate;
    private OffsetDateTime updateDate;
}
