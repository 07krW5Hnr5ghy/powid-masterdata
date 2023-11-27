package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.Store;
import org.springframework.data.domain.Page;
import org.springframework.stereotype.Repository;

@Repository
public interface StoreRepositoryCustom {
    Page<Store> searchForStore(
            String name,
            String user,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize,
            Boolean status);
}
