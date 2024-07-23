package com.proyect.masterdata.repository.impl;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import org.apache.commons.lang3.StringUtils;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Repository;

import com.proyect.masterdata.domain.CategoryProduct;
import com.proyect.masterdata.repository.CategoryProductRepositoryCustom;

import jakarta.persistence.EntityManager;
import jakarta.persistence.PersistenceContext;
import jakarta.persistence.TypedQuery;
import jakarta.persistence.criteria.CriteriaBuilder;
import jakarta.persistence.criteria.CriteriaQuery;
import jakarta.persistence.criteria.Order;
import jakarta.persistence.criteria.Predicate;
import jakarta.persistence.criteria.Root;

@Repository
public class CategoryProductRepositoryCustomImpl implements CategoryProductRepositoryCustom {
    @PersistenceContext(name = "entityManager")
    private EntityManager entityManager;
    @Override
    public Page<CategoryProduct> searchForCategoryProduct(String name, String user, Date registrationStartDate, Date registrationEndDate, Date updateStartDate, Date updateEndDate, String sort, String sortColumn,
                                                          Integer pageNumber, Integer pageSize, Boolean status) {

        CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
        CriteriaQuery<CategoryProduct> criteriaQuery = criteriaBuilder.createQuery(CategoryProduct.class);
        Root<CategoryProduct> itemRoot = criteriaQuery.from(CategoryProduct.class);

        criteriaQuery.select(itemRoot);
        List<Predicate> conditions = predicateConditions(name, user,registrationStartDate, registrationEndDate, updateStartDate, updateEndDate, status, criteriaBuilder, itemRoot);

        if (!StringUtils.isBlank(sort) && !StringUtils.isBlank(sortColumn)) {

            List<Order> categoryProductList = new ArrayList<>();

            if (sort.equalsIgnoreCase("ASC")) {
                categoryProductList = listASC(sortColumn, criteriaBuilder, itemRoot);
            }

            if (sort.equalsIgnoreCase("DESC")) {
                categoryProductList = listDESC(sortColumn, criteriaBuilder, itemRoot);
            }

            criteriaQuery.where(conditions.toArray(new Predicate[] {})).orderBy(categoryProductList);

        } else {

            criteriaQuery.where(conditions.toArray(new Predicate[] {}));
        }

        TypedQuery<CategoryProduct> ordeTypedQuery = entityManager.createQuery(criteriaQuery);
        ordeTypedQuery.setFirstResult(pageNumber * pageSize);
        ordeTypedQuery.setMaxResults(pageSize);

        Pageable pageable = PageRequest.of(pageNumber, pageSize);
        Long count = getOrderCount(name, user,registrationStartDate, registrationEndDate, updateStartDate, updateEndDate, status);
        return new PageImpl<>(ordeTypedQuery.getResultList(), pageable, count);
    }

    private List<Predicate> predicateConditions(
            String name,
            String user,
            Date registrationStartDate,
            Date registrationEndDate,
            Date updateStartDate,
            Date updateEndDate,
            Boolean status,
            CriteriaBuilder criteriaBuilder,
            Root<CategoryProduct> itemRoot) {

        List<Predicate> conditions = new ArrayList<>();

        if (name != null) {
            conditions.add(
                    criteriaBuilder.and(
                            criteriaBuilder.equal(
                                    criteriaBuilder.upper(itemRoot.get("name")), name.toUpperCase())));
        }

        if (user != null) {
            conditions.add(
                    criteriaBuilder.and(
                            criteriaBuilder.equal(
                                    criteriaBuilder.upper(itemRoot.get("tokenUser")), user.toUpperCase())));
        }

        if(registrationStartDate!=null){
            conditions.add(
                    criteriaBuilder.and(
                            criteriaBuilder.greaterThanOrEqualTo(itemRoot.get("registrationDate"),registrationStartDate)
                    )
            );
        }

        if(registrationEndDate!=null){
            conditions.add(
                    criteriaBuilder.and(
                            criteriaBuilder.lessThanOrEqualTo(itemRoot.get("registrationDate"),registrationEndDate)
                    )
            );
        }

        if(updateStartDate!=null){
            conditions.add(
                    criteriaBuilder.and(
                            criteriaBuilder.greaterThanOrEqualTo(itemRoot.get("updateDate"),updateStartDate)
                    )
            );
        }

        if(updateEndDate!=null){
            conditions.add(
                    criteriaBuilder.and(
                            criteriaBuilder.lessThanOrEqualTo(itemRoot.get("updateDate"),updateEndDate)
                    )
            );
        }

        if (status) {
            conditions.add(criteriaBuilder.and(criteriaBuilder.isTrue(itemRoot.get("status"))));
        }

        if (!status) {
            conditions.add(criteriaBuilder.and(criteriaBuilder.isFalse(itemRoot.get("status"))));
        }

        return conditions;
    }

    private List<Order> listASC(
            String sortColumn,
            CriteriaBuilder criteriaBuilder,
            Root<CategoryProduct> itemRoot) {

        List<Order> categoryProductList = new ArrayList<>();

        if (sortColumn.equalsIgnoreCase("NAME")) {
            categoryProductList.add(criteriaBuilder.asc(itemRoot.get("name")));
        }

        if (sortColumn.equalsIgnoreCase("tokenUser")) {
            categoryProductList.add(criteriaBuilder.asc(itemRoot.get("tokenUser")));
        }

        if (sortColumn.equalsIgnoreCase("registrationStartDate")) {
            categoryProductList.add(criteriaBuilder.asc(itemRoot.get("registrationDate")));
        }

        if (sortColumn.equalsIgnoreCase("registrationEndDate")) {
            categoryProductList.add(criteriaBuilder.asc(itemRoot.get("registrationDate")));
        }

        if (sortColumn.equalsIgnoreCase("updateStartDate")) {
            categoryProductList.add(criteriaBuilder.asc(itemRoot.get("updateDate")));
        }

        if (sortColumn.equalsIgnoreCase("updateEndDate")) {
            categoryProductList.add(criteriaBuilder.asc(itemRoot.get("updateDate")));
        }

        return categoryProductList;

    }

    private List<Order> listDESC(
            String sortColumn,
            CriteriaBuilder criteriaBuilder,
            Root<CategoryProduct> itemRoot) {

        List<Order> categoryProductList = new ArrayList<>();

        if (sortColumn.equalsIgnoreCase("NAME")) {
            categoryProductList.add(criteriaBuilder.desc(itemRoot.get("name")));
        }

        if (sortColumn.equalsIgnoreCase("tokenUser")) {
            categoryProductList.add(criteriaBuilder.desc(itemRoot.get("tokenUser")));
        }

        if (sortColumn.equalsIgnoreCase("registrationStartDate")) {
            categoryProductList.add(criteriaBuilder.desc(itemRoot.get("registrationDate")));
        }

        if (sortColumn.equalsIgnoreCase("registrationEndDate")) {
            categoryProductList.add(criteriaBuilder.desc(itemRoot.get("registrationDate")));
        }

        if (sortColumn.equalsIgnoreCase("updateStartDate")) {
            categoryProductList.add(criteriaBuilder.desc(itemRoot.get("updateDate")));
        }

        if (sortColumn.equalsIgnoreCase("updateEndDate")) {
            categoryProductList.add(criteriaBuilder.desc(itemRoot.get("updateDate")));
        }

        return categoryProductList;
    }

    private Long getOrderCount(String name, String user,Date registrationStartDate,Date registrationEndDate,Date updateStartDate,Date updateEndDate, Boolean status) {
        CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
        CriteriaQuery<Long> criteriaQuery = criteriaBuilder.createQuery(Long.class);
        Root<CategoryProduct> itemRoot = criteriaQuery.from(CategoryProduct.class);

        criteriaQuery.select(criteriaBuilder.count(itemRoot));
        List<Predicate> conditions = predicateConditions(name, user,registrationStartDate,registrationEndDate,updateStartDate,updateEndDate, status, criteriaBuilder, itemRoot);
        criteriaQuery.where(conditions.toArray(new Predicate[] {}));
        return entityManager.createQuery(criteriaQuery).getSingleResult();
    }

}
