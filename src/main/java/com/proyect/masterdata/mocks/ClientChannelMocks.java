package com.proyect.masterdata.mocks;

import com.proyect.masterdata.dto.ClientChannelDTO;
import lombok.Getter;

public class ClientChannelMocks {
    private ClientChannelDTO clientChanel1 = ClientChannelDTO.builder()
            .name("compras en linea")
            .url("https://comprasonline.com")
            .user("gm33")
            .status(true)
            .build();

    private ClientChannelDTO clientChanel2 = ClientChannelDTO.builder()
            .name("descuentos en linea")
            .url("https://ediscounts.com")
            .user("lr66")
            .status(true)
            .build();

    private ClientChannelDTO clientChanel3 = ClientChannelDTO.builder()
            .name("swift")
            .url("https://swiftfashion.com")
            .user("ag65")
            .status(true)
            .build();

    private ClientChannelDTO clientChanel4 = ClientChannelDTO.builder()
            .name("oclothes")
            .url("https://oclothes.com")
            .user("ccc45")
            .status(true)
            .build();

    @Getter
    private ClientChannelDTO[] clientChannelList = {clientChanel1,clientChanel2,clientChanel3,clientChanel4};
}
