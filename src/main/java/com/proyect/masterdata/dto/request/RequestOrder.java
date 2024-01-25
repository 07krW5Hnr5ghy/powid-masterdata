package com.proyect.masterdata.dto.request;

import com.proyect.masterdata.domain.Customer;
import com.proyect.masterdata.domain.Item;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Builder
@AllArgsConstructor
@NoArgsConstructor
@Data
public class RequestOrder {

    private String deliveryMan;

    private String deliveryManPhone;

    private RequestSale requestSale;

    private RequestItem[] requestItems;

    private RequestCustomer requestCustomer;
}
