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
    private Long id;
    private ProductDTO product;
    private Integer quantity;
    private Double unitPrice;
    private Double totalPrice;
    private String observations;
}
