package com.proyect.masterdata.dto.request;

import com.proyect.masterdata.domain.Order;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Builder
@AllArgsConstructor
@NoArgsConstructor
@Data
public class RequestItem {
    private Integer quantity;
    private Double discount;
    private String productSku;
}
