package com.proyect.masterdata.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.Date;

@Builder
@AllArgsConstructor
@NoArgsConstructor
@Data
public class OrderStockDTO {
    private Long orderId;
    private Long itemId;
    private String warehouse;
    private String serialSupplierProduct;
    private Integer quantity;
    private Date registrationDate;
    private Date updateDate;
}
