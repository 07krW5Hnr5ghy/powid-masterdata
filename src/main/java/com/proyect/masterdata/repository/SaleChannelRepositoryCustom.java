package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.SaleChannel;
import org.springframework.data.domain.Page;
import org.springframework.stereotype.Repository;

@Repository
public interface SaleChannelRepositoryCustom {
    Page<SaleChannel> searchForSaleChannel(
            String name,
            String user,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize,
            Boolean status
    );
}
