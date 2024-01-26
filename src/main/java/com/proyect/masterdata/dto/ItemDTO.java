package com.proyect.masterdata.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Builder
@AllArgsConstructor
@NoArgsConstructor
@Data
public class ItemDTO {
    private String brand;
    private String category;
    private String color;
    private String model;
    private String sku;
    private String size;
    private Integer quantity;
    private Double unitPrice;
    private Double totalPrice;
    private String observations;
}
