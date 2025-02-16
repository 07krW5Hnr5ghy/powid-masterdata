package com.proyect.masterdata.repository;

import org.springframework.data.domain.Page;
import org.springframework.stereotype.Repository;

import com.proyect.masterdata.domain.Supplier;

import java.util.List;
import java.util.UUID;

@Repository
public interface SupplierRepositoryCustom {
    public Page<Supplier> searchForSupplier(
            UUID clientId,
            List<String> names,
            List<String> rucs,
            List<UUID> countryIds,
            List<UUID> supplierTypeIds,
            List<UUID> departmentIds,
            List<UUID> provinceIds,
            List<UUID> districtIds,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize,
            Boolean status);

}
