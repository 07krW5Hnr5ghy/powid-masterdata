package com.proyect.masterdata.repository.impl;

import java.util.ArrayList;
import java.util.List;

import org.apache.commons.lang3.StringUtils;
import org.springframework.data.domain.Page;
import org.springframework.stereotype.Repository;

import com.proyect.masterdata.domain.Product;
import com.proyect.masterdata.repository.ProductRepositoryCustom;

import jakarta.persistence.EntityManager;
import jakarta.persistence.PersistenceContext;
import jakarta.persistence.TypedQuery;
import jakarta.persistence.criteria.CriteriaBuilder;
import jakarta.persistence.criteria.CriteriaQuery;
import jakarta.persistence.criteria.Order;
import jakarta.persistence.criteria.Predicate;
import jakarta.persistence.criteria.Root;

import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.PageImpl;

@Repository
public class ProductRepositoryCustomImpl implements ProductRepositoryCustom {

    @PersistenceContext(name = "entityManager")
    private EntityManager entityManager;

    @Override
    public Page<Product> searchForProduct(String sku, Long clientId, String sort, String sortColumn, Integer pageNumber,
            Integer pageSize, Boolean status) {

        CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
        CriteriaQuery<Product> criteriaQuery = criteriaBuilder.createQuery(Product.class);

        Root<Product> itemRoot = criteriaQuery.from(Product.class);

        criteriaQuery.select(itemRoot);

        List<Predicate> conditions = predicateConditions(sku, clientId, status, criteriaBuilder, itemRoot);

        if (!StringUtils.isBlank(sort) && !StringUtils.isBlank(sortColumn)) {

            List<Order> productList = new ArrayList<>();

            if (sort.equalsIgnoreCase("ASC")) {
                productList = listASC(sortColumn, criteriaBuilder, itemRoot);
            }

            if (sort.equalsIgnoreCase("DESC")) {
                productList = listDESC(sortColumn, criteriaBuilder, itemRoot);
            }

            criteriaQuery.where(conditions.toArray(new Predicate[] {})).orderBy(productList);

        } else {

            criteriaQuery.where(conditions.toArray(new Predicate[] {}));
        }

        TypedQuery<Product> orderTypedQuery = entityManager.createQuery(criteriaQuery);
        orderTypedQuery.setFirstResult(pageNumber * pageSize);
        orderTypedQuery.setMaxResults(pageSize);

        Pageable pageable = PageRequest.of(pageNumber, pageSize);
        Long count = getOrderCount(sku, clientId, status);
        return new PageImpl<>(orderTypedQuery.getResultList(), pageable, count);
    }

    private List<Predicate> predicateConditions(String sku, Long clientId, Boolean status,
            CriteriaBuilder criteriaBuilder, Root<Product> itemRoot) {

        List<Predicate> conditions = new ArrayList<>();

        if (sku != null) {
            conditions.add(criteriaBuilder
                    .and(criteriaBuilder.equal(criteriaBuilder.upper(itemRoot.get("sku")), sku.toUpperCase())));
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

    private List<Order> listASC(String sortColumn, CriteriaBuilder criteriaBuilder, Root<Product> itemRoot) {

        List<Order> productList = new ArrayList<>();

        if (sortColumn.equalsIgnoreCase("sku")) {
            productList.add(criteriaBuilder.asc(itemRoot.get("sku")));
        }

        if (sortColumn.equalsIgnoreCase("clientId")) {
            productList.add(criteriaBuilder.asc(itemRoot.get("clientId")));
        }

        return productList;
    }

    private List<Order> listDESC(String sortColumn, CriteriaBuilder criteriaBuilder, Root<Product> itemRoot) {

        List<Order> productList = new ArrayList<>();

        if (sortColumn.equalsIgnoreCase("sku")) {
            productList.add(criteriaBuilder.desc(itemRoot.get("sku")));
        }

        if (sortColumn.equalsIgnoreCase("clientId")) {
            productList.add(criteriaBuilder.desc(itemRoot.get("clientId")));
        }

        return productList;
    }

    private Long getOrderCount(String sku, Long clientId, Boolean status) {
        CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
        CriteriaQuery<Long> criteriaQuery = criteriaBuilder.createQuery(Long.class);
        Root<Product> itemRoot = criteriaQuery.from(Product.class);

        criteriaQuery.select(criteriaBuilder.count(itemRoot));
        List<Predicate> conditions = predicateConditions(sku, clientId, status, criteriaBuilder, itemRoot);
        criteriaQuery.where(conditions.toArray(new Predicate[] {}));
        return entityManager.createQuery(criteriaQuery).getSingleResult();
    }

}
