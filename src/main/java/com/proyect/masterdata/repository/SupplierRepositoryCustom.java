package com.proyect.masterdata.repository;

import org.springframework.data.domain.Page;
import org.springframework.stereotype.Repository;

import com.proyect.masterdata.domain.Supplier;

@Repository
public interface SupplierRepositoryCustom {
    public Page<Supplier> searchForSupplier(
            String name,
            String ruc,
            Long clientId,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize,
            Boolean status);

}
