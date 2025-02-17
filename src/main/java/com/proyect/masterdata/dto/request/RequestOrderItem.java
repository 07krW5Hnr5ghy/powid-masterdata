package com.proyect.masterdata.dto.request;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.UUID;

@Builder
@AllArgsConstructor
@NoArgsConstructor
@Data
public class RequestOrderItem {
    private Integer quantity;
    private String discount;
    private Double discountAmount;
    private UUID productId;
    private String observations;
}
