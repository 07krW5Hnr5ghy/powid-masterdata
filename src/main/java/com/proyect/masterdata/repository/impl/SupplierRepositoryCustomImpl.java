package com.proyect.masterdata.repository.impl;

import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

import com.proyect.masterdata.domain.District;
import com.proyect.masterdata.domain.Province;
import jakarta.persistence.criteria.*;
import org.apache.commons.lang3.StringUtils;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Repository;

import com.proyect.masterdata.domain.Supplier;
import com.proyect.masterdata.repository.SupplierRepositoryCustom;

import jakarta.persistence.EntityManager;
import jakarta.persistence.PersistenceContext;
import jakarta.persistence.TypedQuery;

@Repository
public class SupplierRepositoryCustomImpl implements SupplierRepositoryCustom {

    @PersistenceContext(name = "entityManager")
    private EntityManager entityManager;

    @Override
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
            Boolean status) {

        CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
        CriteriaQuery<Supplier> criteriaQuery = criteriaBuilder.createQuery(Supplier.class);

        Root<Supplier> itemRoot = criteriaQuery.from(Supplier.class);
        Join<Supplier, District> supplierDistrictJoin = itemRoot.join("district");
        Join<District, Province> districtProvinceJoin = supplierDistrictJoin.join("province");

        criteriaQuery.select(itemRoot);

        List<Predicate> conditions = predicateConditions(
                clientId,
                names,
                rucs,
                countryIds,
                supplierTypeIds,
                departmentIds,
                provinceIds,
                districtIds,
                status,
                criteriaBuilder,
                itemRoot,
                supplierDistrictJoin,
                districtProvinceJoin);

        if (!StringUtils.isBlank(sort) && !StringUtils.isBlank(sortColumn)) {

            List<Order> supplierList = new ArrayList<>();

            if (sort.equalsIgnoreCase("ASC")) {
                supplierList = listASC(sortColumn, criteriaBuilder, itemRoot);
            }

            if (sort.equalsIgnoreCase("DESC")) {
                supplierList = listDESC(sortColumn, criteriaBuilder, itemRoot);
            }

            criteriaQuery.where(conditions.toArray(new Predicate[] {})).orderBy(supplierList);
        } else {
            criteriaQuery.where(conditions.toArray(new Predicate[] {}));
        }

        TypedQuery<Supplier> orderTypedQuery = entityManager.createQuery(criteriaQuery);
        orderTypedQuery.setFirstResult(pageNumber * pageSize);
        orderTypedQuery.setMaxResults(pageSize);

        Pageable pageable = PageRequest.of(pageNumber, pageSize);
        Long count = getOrderCount(
                clientId,
                names,
                rucs,
                countryIds,
                supplierTypeIds,
                departmentIds,
                provinceIds,
                districtIds,
                status);
        return new PageImpl<>(orderTypedQuery.getResultList(), pageable, count);

    }

    private List<Predicate> predicateConditions(
            UUID clientId,
            List<String> names,
            List<String> rucs,
            List<UUID> countryIds,
            List<UUID> supplierTypeIds,
            List<UUID> departmentIds,
            List<UUID> provinceIds,
            List<UUID> districtIds,
            Boolean status,
            CriteriaBuilder criteriaBuilder,
            Root<Supplier> itemRoot,
            Join<Supplier, District> supplierDistrictJoin,
            Join<District, Province> districtProvinceJoin) {

        List<Predicate> conditions = new ArrayList<>();

        if (!names.isEmpty()) {
            conditions.add(criteriaBuilder.and(itemRoot.get("businessName").in(names)));
        }

        if (!rucs.isEmpty()) {
            conditions.add(criteriaBuilder.and(itemRoot.get("ruc").in(rucs)));
        }

        if(!countryIds.isEmpty()){
            conditions.add(criteriaBuilder.and(itemRoot.get("countryId").in(countryIds)));
        }

        if(!supplierTypeIds.isEmpty()){
            conditions.add(criteriaBuilder.and(itemRoot.get("supplierTypeId").in(supplierTypeIds)));
        }

        if(!departmentIds.isEmpty()){
            conditions.add(criteriaBuilder.and(districtProvinceJoin.get("departmentId").in(departmentIds)));
        }

        if (!provinceIds.isEmpty()){
            conditions.add(criteriaBuilder.and(supplierDistrictJoin.get("provinceId").in(provinceIds)));
        }

        if(!districtIds.isEmpty()){
            conditions.add(criteriaBuilder.and(itemRoot.get("districtId").in(districtIds)));
        }

        if (clientId != null) {
            conditions.add(criteriaBuilder.and(criteriaBuilder.equal(itemRoot.get("clientId"), clientId)));
        }

        if (status) {
            conditions.add(criteriaBuilder.and(criteriaBuilder.isTrue(itemRoot.get("status"))));
        }

        if (!status) {
            conditions.add(criteriaBuilder.and(criteriaBuilder.isFalse(itemRoot.get("status"))));
        }

        return conditions;
    }

    private List<Order> listASC(String sortColumn, CriteriaBuilder criteriaBuilder, Root<Supplier> itemRoot) {

        List<Order> supplierList = new ArrayList<>();

        if (sortColumn.equalsIgnoreCase("name")) {
            supplierList.add(criteriaBuilder.asc(itemRoot.get("businessName")));
        }

        if (sortColumn.equalsIgnoreCase("ruc")) {
            supplierList.add(criteriaBuilder.asc(itemRoot.get("ruc")));
        }

        if (sortColumn.equalsIgnoreCase("clientId")) {
            supplierList.add(criteriaBuilder.asc(itemRoot.get("clientId")));
        }

        return supplierList;

    }

    private List<Order> listDESC(String sortColumn, CriteriaBuilder criteriaBuilder, Root<Supplier> itemRoot) {

        List<Order> supplierList = new ArrayList<>();

        if (sortColumn.equalsIgnoreCase("name")) {
            supplierList.add(criteriaBuilder.desc(itemRoot.get("businessName")));
        }

        if (sortColumn.equalsIgnoreCase("ruc")) {
            supplierList.add(criteriaBuilder.desc(itemRoot.get("ruc")));
        }

        if (sortColumn.equalsIgnoreCase("clientId")) {
            supplierList.add(criteriaBuilder.desc(itemRoot.get("clientId")));
        }

        return supplierList;

    }

    private Long getOrderCount(
            UUID clientId,
            List<String> names,
            List<String> rucs,
            List<UUID> countryIds,
            List<UUID> supplierTypeIds,
            List<UUID> departmentIds,
            List<UUID> provinceIds,
            List<UUID> districtIds,
            Boolean status) {

        CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
        CriteriaQuery<Long> criteriaQuery = criteriaBuilder.createQuery(Long.class);
        Root<Supplier> itemRoot = criteriaQuery.from(Supplier.class);
        Join<Supplier, District> supplierDistrictJoin = itemRoot.join("district");
        Join<District, Province> districtProvinceJoin = supplierDistrictJoin.join("province");

        criteriaQuery.select(criteriaBuilder.count(itemRoot));
        List<Predicate> conditions = predicateConditions(
                clientId,
                names,
                rucs,
                countryIds,
                supplierTypeIds,
                departmentIds,
                provinceIds,
                districtIds,
                status,
                criteriaBuilder,
                itemRoot,
                supplierDistrictJoin,
                districtProvinceJoin);
        criteriaQuery.where(conditions.toArray(new Predicate[] {}));
        return entityManager.createQuery(criteriaQuery).getSingleResult();

    }

}
