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
public class OrderReturnItemDTO {
    private UUID orderId;
    private String supplierProduct;
    private String product;
    private OffsetDateTime registrationDate;
    private OffsetDateTime updateDate;
    private String returnType;
    private String warehouse;
    private Integer quantity;
}
