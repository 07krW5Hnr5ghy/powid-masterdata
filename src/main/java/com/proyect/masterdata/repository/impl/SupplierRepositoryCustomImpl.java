package com.proyect.masterdata.repository.impl;

import java.util.ArrayList;
import java.util.List;

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
import jakarta.persistence.criteria.CriteriaBuilder;
import jakarta.persistence.criteria.CriteriaQuery;
import jakarta.persistence.criteria.Order;
import jakarta.persistence.criteria.Predicate;
import jakarta.persistence.criteria.Root;

@Repository
public class SupplierRepositoryCustomImpl implements SupplierRepositoryCustom {

    @PersistenceContext(name = "entityManager")
    private EntityManager entityManager;

    @Override
    public Page<Supplier> searchForSupplier(String name, String ruc, Long clientId, String sort, String sortColumn,
            Integer pageNumber, Integer pageSize, Boolean status) {

        CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
        CriteriaQuery<Supplier> criteriaQuery = criteriaBuilder.createQuery(Supplier.class);

        Root<Supplier> itemRoot = criteriaQuery.from(Supplier.class);

        criteriaQuery.select(itemRoot);

        List<Predicate> conditions = predicateConditions(name, ruc, clientId, status, criteriaBuilder, itemRoot);

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
        Long count = getOrderCount(name, ruc, clientId, status);
        return new PageImpl<>(orderTypedQuery.getResultList(), pageable, count);

    }

    private List<Predicate> predicateConditions(String name, String ruc, Long clientId, Boolean status,
            CriteriaBuilder criteriaBuilder, Root<Supplier> itemRoot) {

        List<Predicate> conditions = new ArrayList<>();

        if (name != null) {
            conditions.add(criteriaBuilder.and(
                    criteriaBuilder.equal(criteriaBuilder.upper(itemRoot.get("businessName")), name.toUpperCase())));
        }

        if (ruc != null) {
            conditions.add(criteriaBuilder.and(
                    criteriaBuilder.equal(criteriaBuilder.upper(itemRoot.get("ruc")), ruc)));
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

    private Long getOrderCount(String name, String ruc, Long clientId, Boolean status) {

        CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
        CriteriaQuery<Long> criteriaQuery = criteriaBuilder.createQuery(Long.class);
        Root<Supplier> itemRoot = criteriaQuery.from(Supplier.class);

        criteriaQuery.select(criteriaBuilder.count(itemRoot));
        List<Predicate> conditions = predicateConditions(name, ruc, clientId, status, criteriaBuilder, itemRoot);
        criteriaQuery.where(conditions.toArray(new Predicate[] {}));
        return entityManager.createQuery(criteriaQuery).getSingleResult();

    }

}
