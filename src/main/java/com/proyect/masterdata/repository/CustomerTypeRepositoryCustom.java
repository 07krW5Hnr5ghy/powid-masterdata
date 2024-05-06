package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.CustomerType;
import org.springframework.data.domain.Page;
import org.springframework.stereotype.Repository;

@Repository
public interface CustomerTypeRepositoryCustom {
    Page<CustomerType> searchForCustomerType(
            String name,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize,
            Boolean status
    );
}
