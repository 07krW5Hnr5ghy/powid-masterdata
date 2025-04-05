package com.proyect.masterdata.dto.request;

import com.proyect.masterdata.domain.Product;
import com.proyect.masterdata.domain.SupplyOrderItem;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Builder
@AllArgsConstructor
@NoArgsConstructor
@Data
public class RequestKardexOutput {
    private String user;
    private Integer quantity;
    private Product product;
    private SupplyOrderItem supplyOrderItem;
}
