package com.proyect.masterdata.mocks;

import com.proyect.masterdata.dto.StoreDTO;
import lombok.Getter;

public class ClientChannelMocks {
        private StoreDTO clientChanel1 = StoreDTO.builder()
                        .name("compras en linea")
                        .url("https://comprasonline.com")
                        .user("gm33")
                        .status(true)
                        .build();

        private StoreDTO clientChanel2 = StoreDTO.builder()
                        .name("descuentos en linea")
                        .url("https://ediscounts.com")
                        .user("lr66")
                        .status(true)
                        .build();

        private StoreDTO clientChanel3 = StoreDTO.builder()
                        .name("swift")
                        .url("https://swiftfashion.com")
                        .user("ag65")
                        .status(true)
                        .build();

        private StoreDTO clientChanel4 = StoreDTO.builder()
                        .name("oclothes")
                        .url("https://oclothes.com")
                        .user("ccc45")
                        .status(true)
                        .build();

        @Getter
        private StoreDTO[] clientChannelList = { clientChanel1, clientChanel2, clientChanel3, clientChanel4 };
}
