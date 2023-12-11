package com.proyect.masterdata.repository.impl;

import java.util.ArrayList;
import java.util.List;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Repository;

import com.proyect.masterdata.domain.Brand;
import com.proyect.masterdata.domain.Model;
import com.proyect.masterdata.repository.ModelRepositoryCustom;

import io.micrometer.common.util.StringUtils;
import jakarta.persistence.EntityManager;
import jakarta.persistence.PersistenceContext;
import jakarta.persistence.TypedQuery;
import jakarta.persistence.criteria.CriteriaBuilder;
import jakarta.persistence.criteria.CriteriaQuery;
import jakarta.persistence.criteria.Order;
import jakarta.persistence.criteria.Predicate;
import jakarta.persistence.criteria.Root;

@Repository
public class ModelRepositoryCustomImpl implements ModelRepositoryCustom {

    @PersistenceContext(name = "entityManager")
    private EntityManager entityManager;

    @Override
    public Page<Model> searchForModel(String name, Brand brand, String user, String sort, String sortColumn,
            Integer pageNumber,
            Integer pageSize, Boolean status) {

        CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
        CriteriaQuery<Model> criteriaQuery = criteriaBuilder.createQuery(Model.class);
        Root<Model> itemRoot = criteriaQuery.from(Model.class);

        criteriaQuery.select(itemRoot);
        List<Predicate> conditions = predicateConditions(name, brand, user, status, criteriaBuilder, itemRoot);

        if (!StringUtils.isBlank(sort) && !StringUtils.isBlank(sortColumn)) {

            List<Order> modelList = new ArrayList<>();

            if (sort.equalsIgnoreCase("ASC")) {
                modelList = listAsc(sortColumn, criteriaBuilder, itemRoot);
            }

            if (sort.equalsIgnoreCase("DESC")) {
                modelList = listDesc(sortColumn, criteriaBuilder, itemRoot);
            }

            criteriaQuery.where(conditions.toArray(new Predicate[] {})).orderBy(modelList);

        } else {
            criteriaQuery.where(conditions.toArray(new Predicate[] {}));
        }

        TypedQuery<Model> orderTypeQuery = entityManager.createQuery(criteriaQuery);
        orderTypeQuery.setFirstResult(pageNumber * pageSize);
        orderTypeQuery.setMaxResults(pageSize);

        Pageable pageable = PageRequest.of(pageNumber, pageSize);
        long count = getOrderCount(name, brand, user, status);
        return new PageImpl<>(orderTypeQuery.getResultList(), pageable, count);
    }

    public List<Predicate> predicateConditions(
            String name,
            Brand brand,
            String user,
            Boolean status,
            CriteriaBuilder criteriaBuilder,
            Root<Model> itemRoot) {

        List<Predicate> conditions = new ArrayList<>();

        if (name != null) {
            conditions.add(criteriaBuilder.and(
                    criteriaBuilder.equal(criteriaBuilder.upper(itemRoot.get("name")), name.toUpperCase())));
        }

        if (brand != null) {
            conditions.add(criteriaBuilder.and(criteriaBuilder.equal(itemRoot.get("idBrand"), brand.getId())));
        }

        if (user != null) {
            conditions.add(criteriaBuilder.and(
                    criteriaBuilder.equal(criteriaBuilder.upper(itemRoot.get("tokenUser")), user.toUpperCase())));
        }

        if (status) {
            conditions.add(criteriaBuilder.and(criteriaBuilder.isTrue(itemRoot.get("status"))));
        }

        if (!status) {
            conditions.add(criteriaBuilder.and(criteriaBuilder.isFalse(itemRoot.get("status"))));
        }

        return conditions;
    }

    List<Order> listAsc(
            String sortColumn,
            CriteriaBuilder criteriaBuilder,
            Root<Model> itemRoot) {

        List<Order> modelList = new ArrayList<>();

        if (sortColumn.equalsIgnoreCase("NAME")) {
            modelList.add(criteriaBuilder.asc(itemRoot.get("name")));
        }

        if (sortColumn.equalsIgnoreCase("tokenUser")) {
            modelList.add(criteriaBuilder.asc(itemRoot.get("tokenUser")));
        }

        if (sortColumn.equalsIgnoreCase("brandId")) {
            modelList.add(criteriaBuilder.asc(itemRoot.get("brandId")));
        }

        return modelList;
    }

    List<Order> listDesc(
            String sortColumn,
            CriteriaBuilder criteriaBuilder,
            Root<Model> itemRoot) {
        List<Order> modelList = new ArrayList<>();

        if (sortColumn.equalsIgnoreCase("NAME")) {
            modelList.add(criteriaBuilder.desc(itemRoot.get("name")));
        }

        if (sortColumn.equalsIgnoreCase("tokenUser")) {
            modelList.add(criteriaBuilder.desc(itemRoot.get("tokenUser")));
        }

        if (sortColumn.equalsIgnoreCase("brandId")) {
            modelList.add(criteriaBuilder.asc(itemRoot.get("brandId")));
        }

        return modelList;
    }

    private long getOrderCount(String name, Brand brand, String user, Boolean status) {

        CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
        CriteriaQuery<Long> criteriaQuery = criteriaBuilder.createQuery(Long.class);
        Root<Model> itemRoot = criteriaQuery.from(Model.class);

        criteriaQuery.select(criteriaBuilder.count(itemRoot));
        List<Predicate> conditions = predicateConditions(name, brand, user, status, criteriaBuilder, itemRoot);
        criteriaQuery.where(conditions.toArray(new Predicate[] {}));
        return entityManager.createQuery(criteriaQuery).getSingleResult();
    }

}
