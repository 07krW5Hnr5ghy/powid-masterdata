package com.proyect.masterdata.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.UUID;

@Builder
@AllArgsConstructor
@NoArgsConstructor
@Data
public class ChannelListDTO {
    private String name;
    private Integer subscribedMonths;
    private String client;
    private UUID membership;
    private String paymentMethod;
    private String connection;
    private String ecommerce;
    private Integer payedMonths;
    private String user;
}
