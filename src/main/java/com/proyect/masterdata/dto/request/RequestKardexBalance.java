package com.proyect.masterdata.dto.request;

import com.proyect.masterdata.domain.Product;
import com.proyect.masterdata.domain.User;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Builder
@AllArgsConstructor
@NoArgsConstructor
@Data
public class RequestKardexBalance {
    private User user;
    private Product product;
    private Integer quantity;
    private Boolean add;
}
