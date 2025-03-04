package com.proyect.masterdata.dto;

import java.time.OffsetDateTime;
import java.util.Date;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Builder
@AllArgsConstructor
@NoArgsConstructor
@Data
public class PurchaseItemDTO {
    private String warehouse;
    private Integer quantity;
    private String model;
    private String color;
    private String size;
    private Long purchase;
    private OffsetDateTime registrationDate;
}
