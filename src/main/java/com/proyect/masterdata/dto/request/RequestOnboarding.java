package com.proyect.masterdata.dto.request;

import java.util.List;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Builder
@AllArgsConstructor
@NoArgsConstructor
@Data
public class RequestOnboarding {

    private String username;

    private String name;

    private String surname;

    private String email;

    private String address;

    private String mobile;

    private String dni;

    private String category;

    private String users;

    private Boolean ecommerce;

    private Boolean billing;

    private String comment;

    private String businessName;

    private String businessRuc;

    private String gender;

    private String password;

    private String district;

    private String store;

    private String storeUrl;

    private String storeType;

    private List<String> closingChannels;

    private String entryChannel;

    private Boolean demo;

}
