package com.proyect.masterdata.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.time.OffsetDateTime;
import java.util.Date;
import java.util.List;
import java.util.UUID;

@Builder
@AllArgsConstructor
@NoArgsConstructor
@Data
public class OrderItemDTO {
    private UUID orderId;
    private String sku;
    private String model;
    private String color;
    private String size;
    private String category;
    private String subCategory;
    private String unit;
    private Integer quantity;
    private Double unitPrice;
    private String discount;
    private Double discountAmount;
    private Double totalPrice;
    private String observations;
    private List<String> pictures;
    private OffsetDateTime registrationDate;
    private OffsetDateTime updateDate;
}
