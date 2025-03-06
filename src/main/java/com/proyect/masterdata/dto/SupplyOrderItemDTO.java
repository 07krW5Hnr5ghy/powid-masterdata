package com.proyect.masterdata.dto;

import java.time.OffsetDateTime;
import java.util.UUID;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Builder
@AllArgsConstructor
@NoArgsConstructor
@Data
public class SupplyOrderItemDTO {
    private UUID id;
    private Long orderNumber;
    private String ref;
    private String warehouse;
    private Integer quantity;
    private UUID productId;
    private String product;
    private String productSku;
    private String model;
    private String color;
    private String size;
    private OffsetDateTime registrationDate;
    private OffsetDateTime updateDate;
    private String user;
    private Boolean status;
}
