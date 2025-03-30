package com.proyect.masterdata.dto.response;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Builder
@AllArgsConstructor
@NoArgsConstructor
@Data
public class ResponseCourierInfo {
    private String fullName;
    private String phone;
    private String company;
    private String dni;
    private String plate;
    private String address;
    private int ordersDelivered;
    private double collectionBalance;
}
