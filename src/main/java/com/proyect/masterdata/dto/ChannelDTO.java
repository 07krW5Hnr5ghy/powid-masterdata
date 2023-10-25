package com.proyect.masterdata.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Builder
@AllArgsConstructor
@NoArgsConstructor
@Data
public class ChannelDTO {
    private String name;
    private Integer months;
    private String client;
    private Long membership;
    private String paymentMethod;
    private String connection;
    private String user;
}
