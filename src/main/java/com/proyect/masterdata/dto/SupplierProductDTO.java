package com.proyect.masterdata.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.time.OffsetDateTime;
import java.util.Date;

@Builder
@AllArgsConstructor
@NoArgsConstructor
@Data
public class SupplierProductDTO {
    private String serial;
    private String productSku;
    private String model;
    private String color;
    private String size;
    private String supplier;
    private Double price;
    private OffsetDateTime registrationDate;
    private OffsetDateTime updateDate;
}
