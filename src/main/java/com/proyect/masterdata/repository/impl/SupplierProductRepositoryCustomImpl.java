package com.proyect.masterdata.repository.impl;

import com.proyect.masterdata.domain.SupplierProduct;
import com.proyect.masterdata.repository.SupplierProductRepositoryCustom;
import jakarta.persistence.EntityManager;
import jakarta.persistence.PersistenceContext;
import jakarta.persistence.TypedQuery;
import jakarta.persistence.criteria.*;
import org.apache.commons.lang3.StringUtils;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Repository;

import java.util.ArrayList;
import java.util.List;

@Repository
public class SupplierProductRepositoryCustomImpl implements SupplierProductRepositoryCustom {

    @PersistenceContext(name = "entityManager")
    private EntityManager entityManager;

    @Override
    public Page<SupplierProduct> searchForSupplierProduct(String serial, Long clientId,Long productId,Long supplierId,Double purchasePrice, String sort, String sortColumn,
            Integer pageNumber, Integer pageSize, Boolean status) {

        CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
        CriteriaQuery<SupplierProduct> criteriaQuery = criteriaBuilder.createQuery(SupplierProduct.class);
        Root<SupplierProduct> itemRoot = criteriaQuery.from(SupplierProduct.class);

        criteriaQuery.select(itemRoot);

        List<Predicate> conditions = predicateConditions(serial, clientId, productId, supplierId, purchasePrice, status, criteriaBuilder, itemRoot);

        if (!StringUtils.isBlank(sort) && !StringUtils.isBlank(sortColumn)) {

            List<Order> supplierProductList = new ArrayList<>();

            if (sort.equalsIgnoreCase("ASC")) {
                supplierProductList = listASC(sortColumn, criteriaBuilder, itemRoot);
            }

            if (sort.equalsIgnoreCase("DESC")) {
                supplierProductList = listDESC(sortColumn, criteriaBuilder, itemRoot);
            }

            criteriaQuery.where(conditions.toArray(new Predicate[] {})).orderBy(supplierProductList);
        } else {
            criteriaQuery.where(conditions.toArray(new Predicate[] {}));
        }

        TypedQuery<SupplierProduct> orderTypedQuery = entityManager.createQuery(criteriaQuery);
        orderTypedQuery.setFirstResult(pageNumber * pageSize);
        orderTypedQuery.setMaxResults(pageSize);

        Pageable pageable = PageRequest.of(pageNumber, pageSize);
        Long count = getOrderCount(serial, clientId, productId, supplierId, purchasePrice, status);
        return new PageImpl<>(orderTypedQuery.getResultList(), pageable, count);
    }

    private List<Predicate> predicateConditions(
            String serial,
            Long clientId,
            Long productId,
            Long supplierId,
            Double purchasePrice,
            Boolean status,
            CriteriaBuilder criteriaBuilder,
            Root<SupplierProduct> itemRoot) {

        List<Predicate> conditions = new ArrayList<>();

        if (serial != null) {
            conditions.add(criteriaBuilder
                    .and(criteriaBuilder.equal(criteriaBuilder.upper(itemRoot.get("serial")), serial.toUpperCase())));
        }

        if (clientId != null) {
            conditions.add(criteriaBuilder
                    .and(criteriaBuilder.equal(itemRoot.get("clientId"), clientId)));
        }

        if (productId != null) {
            conditions.add(criteriaBuilder
                    .and(criteriaBuilder.equal(itemRoot.get("productId"), productId)));
        }

        if (supplierId != null) {
            conditions.add(criteriaBuilder
                    .and(criteriaBuilder.equal(itemRoot.get("supplierId"), supplierId)));
        }

        if (purchasePrice != null) {
            conditions.add(criteriaBuilder
                    .and(criteriaBuilder.equal(itemRoot.get("purchasePrice"), purchasePrice)));
        }

        if (status) {
            conditions.add(criteriaBuilder.and(criteriaBuilder.isTrue(itemRoot.get("status"))));
        }

        if (!status) {
            conditions.add(criteriaBuilder.and(criteriaBuilder.isFalse(itemRoot.get("status"))));
        }

        return conditions;

    }

    private List<Order> listASC(String sortColumn, CriteriaBuilder criteriaBuilder, Root<SupplierProduct> itemRoot) {

        List<Order> supplierProductList = new ArrayList<>();

        if (sortColumn.equalsIgnoreCase("serial")) {
            supplierProductList.add(criteriaBuilder.asc(itemRoot.get("serial")));
        }

        if (sortColumn.equalsIgnoreCase("clientId")) {
            supplierProductList.add(criteriaBuilder.asc(itemRoot.get("clientId")));
        }

        if (sortColumn.equalsIgnoreCase("productId")) {
            supplierProductList.add(criteriaBuilder.asc(itemRoot.get("productId")));
        }

        if (sortColumn.equalsIgnoreCase("supplierId")) {
            supplierProductList.add(criteriaBuilder.asc(itemRoot.get("supplierId")));
        }

        if (sortColumn.equalsIgnoreCase("purchasePrice")) {
            supplierProductList.add(criteriaBuilder.asc(itemRoot.get("purchasePrice")));
        }

        return supplierProductList;
    }

    private List<Order> listDESC(String sortColumn, CriteriaBuilder criteriaBuilder, Root<SupplierProduct> itemRoot) {

        List<Order> supplierProductList = new ArrayList<>();

        if (sortColumn.equalsIgnoreCase("serial")) {
            supplierProductList.add(criteriaBuilder.desc(itemRoot.get("serial")));
        }

        if (sortColumn.equalsIgnoreCase("clientId")) {
            supplierProductList.add(criteriaBuilder.desc(itemRoot.get("clientId")));
        }

        if (sortColumn.equalsIgnoreCase("productId")) {
            supplierProductList.add(criteriaBuilder.desc(itemRoot.get("productId")));
        }

        if (sortColumn.equalsIgnoreCase("supplierId")) {
            supplierProductList.add(criteriaBuilder.desc(itemRoot.get("supplierId")));
        }

        if (sortColumn.equalsIgnoreCase("purchasePrice")) {
            supplierProductList.add(criteriaBuilder.desc(itemRoot.get("purchasePrice")));
        }

        return supplierProductList;
    }

    private Long getOrderCount(String serial, Long clientId,Long productId, Long supplierId, Double purchasePrice, Boolean status) {

        CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
        CriteriaQuery<Long> criteriaQuery = criteriaBuilder.createQuery(Long.class);
        Root<SupplierProduct> itemRoot = criteriaQuery.from(SupplierProduct.class);

        criteriaQuery.select(criteriaBuilder.count(itemRoot));
        List<Predicate> conditions = predicateConditions(serial, clientId, productId, supplierId, purchasePrice, status, criteriaBuilder, itemRoot);
        criteriaQuery.where(conditions.toArray(new Predicate[] {}));
        return entityManager.createQuery(criteriaQuery).getSingleResult();

    }

}
