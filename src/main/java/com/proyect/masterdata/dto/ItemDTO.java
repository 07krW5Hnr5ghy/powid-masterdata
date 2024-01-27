package com.proyect.masterdata.dto;

import com.proyect.masterdata.domain.Product;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Builder
@AllArgsConstructor
@NoArgsConstructor
@Data
public class ItemDTO {
    private Product product;
    private Integer quantity;
    private Double unitPrice;
    private Double totalPrice;
    private String observations;
}
