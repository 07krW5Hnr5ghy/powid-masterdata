package com.proyect.masterdata.repository;

import org.springframework.data.domain.Page;
import org.springframework.stereotype.Repository;

import com.proyect.masterdata.domain.Supplier;

import java.util.List;

@Repository
public interface SupplierRepositoryCustom {
    public Page<Supplier> searchForSupplier(
            Long clientId,
            List<String> names,
            List<String> rucs,
            List<Long> countryIds,
            List<Long> supplierTypeIds,
            List<Long> departmentIds,
            List<Long> provinceIds,
            List<Long> districtIds,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize,
            Boolean status);

}
