package com.proyect.masterdata.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;

@Builder
@AllArgsConstructor
@NoArgsConstructor
@Data
public class OrderItemDTO {
    private Long id;
    private String sku;
    private String color;
    private String size;
    private String category;
    private String unit;
    private Integer quantity;
    private Double unitPrice;
    private Double discount;
    private Double totalPrice;
    private String observations;
    private List<String> pictures;
}
