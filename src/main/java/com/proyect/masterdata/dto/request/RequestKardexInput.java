package com.proyect.masterdata.dto.request;

import com.proyect.masterdata.domain.Product;
import com.proyect.masterdata.domain.SupplyOrderItem;
import com.proyect.masterdata.domain.User;
import com.proyect.masterdata.domain.Warehouse;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Builder
@AllArgsConstructor
@NoArgsConstructor
@Data
public class RequestKardexInput {
    private User user;
    private Product product;
    private String kardexOperationType;
    private Warehouse warehouse;
    private Integer quantity;
    private Double unitPrice;
    private Double unitValue;
}
